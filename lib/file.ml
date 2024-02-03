let read_file fname = In_channel.with_open_text fname In_channel.input_all

let write_file fname s =
  Out_channel.with_open_text fname (fun oc -> Out_channel.output_string oc s)
