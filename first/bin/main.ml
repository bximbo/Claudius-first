open Claudius

(* ----- *)

type point = {
  x : float ;
  y : float ;
  z : float ;
}

let rotate_x (a : float) (p : point) : point =
  { p with
    y = (p.y *. (cos (a))) -. (p.z *. sin(a)) ;
    z = (p.y *. sin(a)) +. (p.z *. cos(a)) ;
  }

let rotate_y (a : float) (p : point) : point =
  { p with
    x = (p.x *. cos(a)) -. (p.z *. sin(a)) ;
    z = (p.x *. sin(a)) +. (p.z *. cos(a)) ;
  }

let rotate_z (a : float) (p : point) : point =
  { p with
    x = (p.x *. cos(a)) -. (p.y *. sin(a)) ;
    y = (p.x *. sin(a)) +. (p.y *. cos(a)) ;
  }

let point_z_cmp (a : point) (b : point) : int =
  if a.z == b.z then 0
  else if a.z < b.z then 1 else -1

(* ----- *)

let generate_sphere (ft : float) : point list =
  let slices = 18
  and lats = 8
  and radius = 30.
  and offset = 0.
  and max_dots_per_lat = 60.
  and dots_per_slice = 31 in
  let nested_slices = Array.init slices (fun s ->
    let slice_angle = (2. *. (Float.of_int s) *. Float.pi /. (Float.of_int slices)) in
    Array.init dots_per_slice (fun i ->
      let fi = Float.of_int i in
      let a = (2. *. fi *. Float.pi /. (Float.of_int dots_per_slice)) in
      {
        x = (radius +. offset) *. cos a ;
        y = (radius +. offset) *. sin a ;
        z = 0. ;
      }
      |> rotate_y (slice_angle)
    )
  ) in
  let nested_lats = Array.init lats (fun lat ->
    let lh = radius *. cos (((Float.of_int lat) *. Float.pi) /. (Float.of_int lats)) in
    let dots_per_lat = Int.of_float(max_dots_per_lat *.sin (((Float.of_int lat) *. Float.pi) /. (Float.of_int lats))) in
    Array.init dots_per_lat (fun l ->
      let fl = Float.of_int l in
      let r = radius *. sin (((Float.of_int lat) *. Float.pi) /. (Float.of_int lats)) in
      let a = (2. *. fl *. Float.pi /. (Float.of_int dots_per_lat)) in
      {
        x = (r +. offset) *. cos a ;
        y = lh ;
        z = (r +. offset) *. sin a ;
      }
    )
  ) in
  let ring_width = 10
  and inner_radius = 40
  and ring_spacing = 2 in
  let nested_rings = Array.init ring_width (fun ring ->
    let r = Float.of_int(inner_radius + (ring * ring_spacing)) in
    let dots_per_ring = 200 + (5 * ring * ring_spacing) in
      
    (* I defined the spinning speed of the rings here *)
    let theta = ft *. 0.06 in  (* This controls how fast the rings spin *)

    Array.init dots_per_ring (fun d ->
      let fd = Float.of_int d in
      let a = (2. *. fd *. Float.pi /. (Float.of_int dots_per_ring)) +. ft in
      {
        x = (r +. offset) *. cos (a +. theta);  (* Apply rotation *)
        y = 0. ;
        z = (r +. offset) *. sin (a +. theta);  (* Apply rotation *)
      }
    )
  ) in
  let lested = Array.to_list (Array.append (Array.append nested_slices nested_lats) nested_rings) in
  Array.to_list (Array.concat lested)

let render_to_primitives (ft : float) (s : Screen.t) (points : point list) : Primitives.t list =
  let width, height = Screen.dimensions s
  and palette = Screen.palette s in
  let m = 2000. +. cos(ft /. 30.) *. 600. in
  let center_x, center_y = 0., 0. in  (* New: Fixed visual center *)
  List.map (fun e ->
    Primitives.Pixel ({
      x = ((width / 2) + int_of_float(m *. (e.x -. center_x) /. (e.z +. 400.))); 
      y = ((height / 2) + int_of_float(m *. (e.y -. center_y) /. (e.z +. 400.)));
    }, ((Palette.size palette) - 1))
  ) points

(* ----- *)

let tick (t : int) (s : Screen.t) (prev : Framebuffer.t) (_inputs : Base.KeyCodeSet.t): Framebuffer.t =
  (*let color = (0 + t) mod 31 + 10 in*)
  let buffer = Framebuffer.map (fun pixel ->
    if pixel > 36 then (pixel-1) 
    else if pixel < 31 then (pixel +1)
    else ((0 + t) mod 40 - 10)
  ) prev in

  let ft = Float.of_int t in

  generate_sphere ft
  |> List.map (fun p ->
    rotate_y (0.02 *. ft) p |> rotate_x (0.01 *. ft) |> rotate_z (0.005 *. ft)
  )
  |> List.sort point_z_cmp
  |> render_to_primitives ft s
  |> Framebuffer.render buffer;

  buffer

(* ----- *)




(*
let () =
  Screen.create 640 480 1 (Palette.generate_mono_palette 16) |>
  Base.run "Bimbo's first Example EditEdit🚀: Particle in free space" None tick
  *)
(*
let shades_of_gray = [
  0x393939;  (* Dark gray *)
  0x1C1C1C;  (* Very dark gray *)
  0x080808;  (* Nearly black *)
  0x020202;  (* Almost black *)
  0x000000;  (* Black *)
]

let palette =
  (List.rev (Palette.to_list (Palette.generate_plasma_palette 31))) @ 
  shades_of_gray   *)




let shades_of_gray =
  let step = 255 / 8 in
  List.init 9 (fun i ->  
    let value = 255 - (i * step) in 
    (value lsl 16) lor (value lsl 8) lor value  
  )


let palette =
  (List.rev (Palette.to_list (Palette.generate_plasma_palette 31))) @
  shades_of_gray  


let () =
  Palette.of_list palette |>
  Screen.create 689 489 1 |>
  Base.run "🚀Orbit-like Particle in Dynamic Free Space🚀" None tick