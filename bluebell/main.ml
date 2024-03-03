open! Core
open Async

let command =
  Command.async ~summary:"entrypoint"
    ~readme:(fun () -> "asdf")
    (let%map_open.Command path = anon ("source" %: string) in
     fun () ->
       Reader.with_file path ~f:(fun reader ->
           let%bind contents =
             Pipe.read_all (Reader.pipe reader)
             >>| Queue.to_list >>| String.concat
           in
           let%bind results = Nanopass_runner.compile ~contents in
           print_s [%message "compilation result" (results : string Or_error.t)];
           return ()))
