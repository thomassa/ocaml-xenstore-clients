(* A place to put client library tests *)

module Client = Xs_client_unix.Client(Xs_transport_unix_client)



let test_broken_callback () =
  let client = Client.make () in
  let m = ref 0 in
  Client.set_logger (fun s -> incr m; Printf.fprintf stderr "This error is not a failure: %s" s);
  let n = ref 0 in
  let watch_callback _ =
    incr n;
    failwith "Error"
  in
  Client.set_watch_callback client watch_callback;

  Client.immediate client (fun xs ->
    Client.watch xs "/tmp" "");
  
  Thread.delay 1.0;

  Client.immediate client (fun xs ->
    Client.write xs "/tmp" "foo");

  Thread.delay 1.0;

  if !n <> 2 && !m <> 2 then failwith "Test failed"

let test_watch_callbacks () =
  let client = Client.make () in
  let finished = ref false in

  let watch_callback _ =
    let domid = 
      Client.immediate client (fun xs ->
	Client.read xs "domid") in
    finished := true;
    Printf.printf "Read domid: %s\n" domid
  in

  Client.set_watch_callback client watch_callback;

  Client.immediate client (fun xs ->
    Client.watch xs "/tmp" "");

  Thread.delay 5.0;
  
  if not !finished then
    failwith "Test failed"

let test_overflow_callback () =
  let client = Client.make () in
  let c = Condition.create () in
  let m = Mutex.create () in
  let finished = ref false in
  let watch_callback _ =
    with_mutex m (fun () ->
      Printf.printf "watch_callback about to wait.";
      Condition.wait c m;
      Printf.printf "watch_callback finishing."
    )
  in

  let overflow_callback () =
    Printf.printf "overflow_callback starting.";
    finished := true
  in

  Client.set_watch_callback client watch_callback;
  Client.set_watch_overflow_callback client overflow_callback;

  Client.immediate client (fun xs ->
    Client.watch xs "/tmp" "");
  for i = 0 to 66600 do
    Client.immediate client (fun xs ->
      Client.write xs "/tmp" (string_of_int i)
    )
  done;
  with_mutex m (fun () -> Condition.signal c);
  Thread.delay 5.0;
  if not !finished then
    failwith "Overflow-callback test failed."

let _ =
  test_watch_callbacks ();
  test_overflow_callback ();
  test_broken_callback ()
