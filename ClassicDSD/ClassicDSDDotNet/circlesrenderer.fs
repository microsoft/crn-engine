// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.DNA.CirclesRenderer
open Microsoft.Research.DNA.LogicDSD
open Microsoft.Research.DNA.Species
open Microsoft.Research.CRNEngine.Svg

// FP: I am adapting this from the version in SiteGraphReactor.

//---------------------------
// Circle based visualisation
// (NuPack-style)
// From DnaModelling solution, DnaStructures project, Visualizations.fs file
//---------------------------

(* Algorithm:
    - build a circle with edges
    - collapse edges until they are all turned into children
    - collapse edges in children recursively
*)

type svg_domain = 
    | Domain of DomainC
    | Xing of DomainC
    | Hole
    member this.text = 
        match this with
        | Domain dom -> dom.ToString().Replace("^", "")
        | Xing _ -> "X"
        | Hole -> "O"
    member this.compl = 
        match this with 
        | Domain dom -> Domain { dom with isComplementary = not dom.isComplementary }
        | _ -> this

/// Represents a circle. The circle is made of ports (which mostly correspond to domains), and may have children (which are other circles connected to it). A circle may be itself the child of another circle, but does not maintain a reference to its parent.
type circle =
  /// The center of the circle.
  { center   : float * float
  /// A rotation to be applied to the circle (to align its ports correctly).
  ; angle    : float
  /// The circle radius.
  ; radius   : float // Invariant: radius = ports.Length |> circle_radius
  /// The ports that make up the circle.
  ; ports    : svg_domain[]
  /// The circle's children. The type of a child is a tuple of: an integer which is the index in the port sequence on which the child is connected to the parent; a domain which is the edge domain connecting the two circles, and the actual child circle.
  ; children : (int * svg_domain * circle) list
  /// Edges within the circle, i.e. sites that ought to connect. If there are edges, then the circle can be collapsed further.
  ; edges    : (int*int) []
  }

let pi = System.Math.PI
let twopi = 2.0 * pi

let min_side_length = 100.0
let xing_length_per_char = 30.0
let circle_has_overhangs ports = Array.exists (fun d -> match d with Domain d -> true | _ -> false) ports
/// Determines the radius of a circle, based on the ports (domains) in it. The radius is determined so that each arc is min_side_length units long.
let circle_radius ports =
  match ports with [|Xing _; Xing _|] -> 4.0
                 | _ -> 0.5*(min_side_length * float ports.Length)/twopi * (if circle_has_overhangs ports then 1.0 else 0.4)

/// Make crossings longer when domain names are long.
let xing_side_length (b:svg_domain) =
  match b with
  | Hole ->
    min_side_length
  | Xing dom
  | Domain dom ->
    let l = float dom.name.Length * xing_length_per_char
    max l min_side_length

/// Normalize the tuple so that the smaller int goes first.
let sort_e (p1:int, p2:int) = min p1 p2, max p1 p2

/// This function receives a circle and an edge within the circle (in the form of a couple of indexes within the circle's ports array). It returns the same circle, except that now it has an additional child. The ports array is split between the parent and the child, and each side of the edge becomes a crossing port, where they join.
let circle_split (p1, p2) (c:circle) = // Do not split on an edge with a right neighbour
  /// The ports, sorted so that p1 is guaranteeed to be the lowest.
  let p1, p2 = sort_e (p1,p2)
  /// The length (in sites count) of the current circle.
  let n = c.ports.Length
  /// The length of the first new circle (parent).
  let n1 = n - (p2-p1)
  /// The length of the second new circle (child).
  let n2 = p2-p1
  /// This function takes the index of a site in the new parent circle, and returns the index of the corresponding site in the original circle.
  let m1_inv i = if i < p1 then i
                 else i-p1+p2 // Note that if i == p1 this will return the index of one side of the edge.
  /// This function takes the index of a site in the original circle, and returns the index of the corresponding site in the new parent circle. Note that this will return a negative number if the site in the original circle is not actually in the new parent circle (i.e. it's in the child circle).
  let m1 i = if i < p1 then i
             else i+p1-p2 // Note that if i == p1 this will return the index of one side of the edge.
  /// This function takes the index of a site in the child circle, and returns the index of the corresponding site in the original circle. Note that the child circle is formed of the sites originally contained between the edge sites; this is why this function doesn't need to take p2 into account.
  let m2_inv i = i+p1+1
  /// This function takes the index of a site in the original circle, and returns the index of the corresponding site in the new child circle. Note that the child circle is formed of the sites originally contained between the edge sites; this is why this function doesn't need to take p2 into account. Note that this will return a negative number if the site in the original circle is not actually in the new child circle (i.e. it's in the parent circle).
  let m2 i = i-p1-1
  /// Turns a Domain port into a Xing port. Note: if this gets called on a port that's not a Domain, then something went wrong in the earlier phases (edges can only logically be between Domain ports).
  let make_xing port = match port with Domain dom -> Xing dom | _ -> failwith ""
  /// The ports of the new parent circle. In the new parent circle's index, the crossing is at index p1.
  let ports1 = Array.init n1 (fun i -> let port = c.ports.[m1_inv i]
                                       if i = p1 then make_xing port
                                       else port)
  /// The ports of the new child circle. In the new parent circle's index, the crossing is always at the last site (i.e. index n2-1).
  let ports2 = Array.init n2 (fun i -> let port = c.ports.[m2_inv i]
                                       if i = n2-1 then make_xing port
                                       else port)
  /// Takes a port index (in the original circle). Returns true if this port will end up in the new parent circle. If called on one of the edge ports, will always return false.
  let in_c1 p = p < p1 || p > p2
  /// Divide any existing children between the two circles, according to which circle the child's port belongs to. Note that this assumes that no children is at p1 or p2 (this should be impossible).
  let cs1, cs2 = c.children |> List.partition (fun (p,_,_) -> in_c1 p)
  /// This function takes as input the characteristics of a parent circle (port count and radius); the connection port on the parent side; the characteristics of a child circle. It returns the coordinates and rotation angle of the child circle. This will be used both to place the new child relative to the parent, and to reposition any pre-existing children of both of them.
  let port_circle (pn, pr) (ports:svg_domain[]) (p:int) (cn, cr) =
    let side_length = xing_side_length ports.[p]
    let angle = (float p / float pn) * twopi
    let d = pr + 0.5 * side_length + cr
    let center = (d * cos angle, d * sin angle)
    let angle2X = -twopi / float cn
    ( center
    , angle - angle2X + pi )

  /// Radius of the new parent circle.
  let radius1 = circle_radius ports1
  /// Radius of the new child circle.
  let radius2 = circle_radius ports2
  
  /// Takes a pre-existing child of the parent circle, and the port on which it is connected to the parent. Returns the same child, translated and rotated into a position that matches the new shape of the parent circle.
  let child1 p ch =
    let center, angle = port_circle (n1, radius1) ports1 p (ch.ports.Length, ch.radius)
    { ch with center = center; angle = angle }
  /// Remap the pre-existing children of the new parent circle, so that they are correctly placed.
  let children1 = cs1 |> List.map (fun (p,s,ch) -> m1 p, s, child1 (m1 p) ch)

  /// Location (center, angle) of the new child circle.
  let center2, angle2 = port_circle (n1, radius1) ports1 p1 (n2, radius2)

  /// Takes a pre-existing child of the new child circle, and the port on which it is connected to the parent. Returns the same child, translated and rotated into a position that matches the new shape of its parent circle (which is the new child circle).
  let child2 p ch =
    let center, angle = port_circle (n2, radius2) ports2 p (ch.ports.Length, ch.radius)
    { ch with center = center; angle = angle }
  /// Remap the pre-existing children of the new child circle, so that they are correctly placed.
  let children2 = cs2 |> List.map (fun (p,s,ch) -> m2 p, s, child2 (m2 p) ch)

  /// Divide the edges of the original circle between the two new circles. Note that this looks at the first site alone because we assume no crossings; if there are crossings, the algorithm will produce wrong results. Also, the edge currently being handled will end up in the child circle at this stage (but we'll remove it soon).
  let es1, es2 = c.edges |> Array.partition (fst >> in_c1)
  /// Map the site indexes of the edges that end up in the parent circle, so that they are in the space of its sites.
  let edges1 = es1 |> Array.map (fun (i,j) -> m1 i, m1 j)
  /// Map the site indexes of the edges that end up in the child circle, so that they are in the space of its sites. Also, remove the edge currently being handled.
  let edges2 = es2 |> Array.filter (fun e -> sort_e e <> (p1,p2))
                   |> Array.map (fun (i,j) -> m2 i, m2 j)
  
  /// The new child circle.
  let circle2 =
    { center   = center2
    ; angle    = angle2
    ; radius   = radius2
    ; ports    = ports2
    ; children = children2
    ; edges    = edges2 }  

  // The parent circle is different from the old circle, because sites have been removed from it. Therefore, its own position and angle relative to its own parent need to get changed.

  let angleX = -twopi / float n
  let angle1X = -twopi / float n1
  let angle1 = c.angle + angleX - angle1X

  let Xx, Xy = c.radius * cos (c.angle + angleX), c.radius * sin (c.angle + angleX)
  let X1x, X1y = radius1 * cos (angle1 + angle1X), radius1 * sin (angle1 + angle1X)
  let center1 = fst c.center + Xx - X1x, snd c.center + Xy - X1y

  { center   = center1
  ; angle    = angle1
  ; radius   = radius1
  ; ports    = ports1
  ; children = (p1, c.ports.[p1], circle2)::children1
  ; edges    = edges1 
  }

/// Collapse all edges in the given circle; each collapse will cause the circle to be split, generating a child circle, joined by a crossing.
let rec collapse_circle (c:circle) =
  if c.edges.Length = 0 then
    // If this circle has no edges, then recursively collapse all children.
    let children = c.children |> List.map (fun (i, s, ch) -> (i, s, collapse_circle ch))
    { c with children = children }
  else
    // If this circle has edges, collapse the first and then recurse on the resulting circle.
    c |> circle_split c.edges.[0] |> collapse_circle

/// Padding around a rendering.
let pad = 12.0

/// Translates the circle so that the entire figure is in the first quadrant. Also returns the width and height of the figure.
let into_view (c:circle) =
  /// Returns an enumeration of the corners of the circle and its children (in this context, by corner of a circle, we mean a corner of a box that contains the circle). Returned corners are in the top-level space; in order to do this, I provide the function with the angular and location delta of the current child.
  let rec corners da (dx, dy) (c:circle) =
    let rotate (x,y) =
      ( x * cos da - y * sin da
      , x * sin da + y * cos da )
    let translate (x,y) = (x + dx, y + dy)
    let transform = rotate >> translate
    seq {
      // Transform the center and return its corners.
      let x, y =  c.center |> transform
      yield (x + c.radius * 2.0, y + c.radius * 2.0)
      yield (x - c.radius * 2.0, y - c.radius * 2.0)
      // Recurse on the children, passing along the parent's delta angle and position.
      yield! c.children |> Seq.collect (fun (i,_,ch) -> corners (c.angle+da) (x, y) ch)
    }
  // Start the enumeration on the topmost circle, which has angle 0 and is centered on the origin.
  let corners = corners 0.0 (0.0, 0.0) c
  // Determine the corners of the box that contains all corners (this could be done more efficiently, but it's very unlikely to be a long enumeration anyway).
  let min_x = corners |> Seq.map fst |> Seq.min
  let max_x = corners |> Seq.map fst |> Seq.max
  let min_y = corners |> Seq.map snd |> Seq.min
  let max_y = corners |> Seq.map snd |> Seq.max
  // Determine the width and height of the box.
  let width = max_x - min_x
  let height = max_y - min_y
  // Translate the center of the topmost circle so that the entire figure is in positive coordinates. Add some padding, too.
  let cx, cy = c.center
  ( { c with center = (cx - min_x + pad, cy - min_y + pad) }
  , width + 2.0*pad
  , height + 2.0*pad )

/// Size of an empty overhang, in site units (e.g. 0.2 means it's 20% the length of a regular domain).
let overhang_to_hole = 0.2

/// Generates the SVG string for the given circle. The supplied style, scale, width and height are not used, but they are integrated in the resulting string.
let circle_to_svg style scale (c:circle, width, height) =
  /// Generates the SVG class for the given domain (so that it can be styled), for a text element.
  let dom_class (dom:svg_domain) = match dom with Hole -> "" | Domain dom | Xing dom -> if dom.isToehold then sprintf "svgdsd dom_%s" dom.name else ""
  /// Converts a circle to SVG; note that this will be recursed over the circle's children. Parameters include the delta angle and position from the parent; these need to be applied to the circle's SVG elements.
  let rec convert da (dx,dy) (c:circle) =
    // Each site will get an equal portion of the circle.
    let n = c.ports.Length
    /// This is the size in radians of the arc for a site.
    let a = twopi / (float n)
    /// Rotates a point by the delta angle.
    let rotate (x,y) =
      ( x * cos da - y * sin da
      , x * sin da + y * cos da )
    /// Translates a point by the delta position.
    let translate (x,y) = (x + dx, y + dy)
    /// Transforms a point by the delta angle and position; this brings it into the correct reference for rendering.
    let transform = rotate >> translate
    /// Transforms a site index into a position on the circle. Note that the size index may be fractional in this context (so we can get positions between sites).
    let pos i =
      let x, y = c.center
      let v = a * i + c.angle
      ( x + c.radius*cos v
      , y + c.radius*sin v) |> transform
    /// Makes an arc between two positions on the circle, specified as site indexes (e.g. 0.5 indicates a point on the circle between the center of the first two sites). The domain passed as a parameter determines the CSS class of the arc.
    let rim v1 v2 b =
      let x1, y1 = v1 |> pos
      let x2, y2 = v2 |> pos
      let v = a * 0.5*(v1+v2) + c.angle + da
      let r = v * 360.0 / twopi
      sprintf "<path class=\"%s\" d=\"M %f,%f A %f,%f %f 0, 1 %f, %f\"/>" (dom_class b) x1 y1 c.radius c.radius r x2 y2
    /// Draws the arc for a normal domain. It covers 0.5 site units to either side of the domain site.
    let arc i b = rim (float i - 0.5) (float i + 0.5) b
    /// Draws the label for a normal domain.
    let label i b =
      // Determine the position of the site on the circle.
      let x, y = i |> float |> pos
      // Determine the angular position of the site, in radians.
      let v = a * float i + c.angle + da
      // Determine the rotation of the text, in degrees (for SVG).
      let r = 90.0 + v * 360.0 / twopi
      // Output the text element. It is at the same position as the center of the site, except it's rotated by 90°.
      sprintf "<text class=\"%s\" x=\"%f\" y=\"%f\" transform=\"rotate(%f %f,%f)\" dy=\"-4\">%s</text>" (dom_class b) x y r x y b.text
    /// Experimental: in a "degenerate" circle (one where all domains are either crossings or holes), hole terminators will get drawn along the crossing axis, rather than along the circle.
    let degenerate = not (circle_has_overhangs c.ports)
    let port_width = 4.0
    /// Generates the SVG string for a port.
    let port i prev b next =
      // The port can be a domain, a hole, or a crossing.
      match b with
      | Hole ->
        // Figure out where the neighboring overhangs start. If they were Xing type domains, then this hole needs to be wider. This can happen on either or both sides, or none.
        let overhang_dist_prev = match prev with Xing _ -> (1.0 - overhang_to_hole) | _ -> 0.5
        let overhang_dist_next = match next with Xing _ -> (1.0 - overhang_to_hole) | _ -> 0.5
        // This is a hole. It gets rendered as a perpendicular dash at the start of the next port, plus an arrow at the end of the previous port.
        let l = 3.0
        // Draw the arrow. Note that this needs to be styled as the _previous_ domain.
        // Determine the arrow's vector, expressed in radians. This can be determined as the angular position of the terminator on the circle. In the degenerate case, the angular position of the previous domain is used instead, plus a 90° rotation to make it point towards the center (this way, it points along the crossing's axis).
        let arrv = if degenerate then
                     a * (float i - 1.0) + c.angle + da + 0.5*pi
                   else
                     a * (float i - overhang_dist_prev) + c.angle + da
        // Determine the arrow's placement. This is obtained by taking the angular position of the terminator on the circle, and using the 'pos' function to convert it to Cartesian. In the degenerate case, we use the angular position of the previous domain (plus the port width, converted to an angular distance).
        let arrx, arry = if degenerate then
                           float i - 1.0 + (asin (port_width / c.radius) / a) |> pos
                         else
                           float i - overhang_dist_prev |> pos
        let arr1x, arr1y = arrx - l * cos arrv, arry - l * sin arrv
        let arr2x, arr2y = arrx + l * cos arrv, arry + l * sin arrv
        let arr3x, arr3y = arrx + 3.0 * l * cos (arrv + 0.5*pi), arry + 3.0 * l * sin (arrv + 0.5*pi)
        let arr = sprintf "<path class=\"%s\" d=\"M %f,%f L %f,%f L %f, %f z\"/>" (dom_class prev) arr1x arr1y arr2x arr2y arr3x arr3y
        // Draw the dash. Note that this needs to be styled as the _next_ domain.
        // Determine the dash's vector, expressed in radians. This can be determined as the angular position of the terminator on the circle. In the degenerate case, the angular position of the next domain is used instead, plus a 90° rotation to make it point towards the center (this way, it points along the crossing's axis).
        let ev = if degenerate then 
                   a * (float i + 1.0) + c.angle + da - 0.5*pi
                 else
                   a * (float i + overhang_dist_next) + c.angle + da
        // Determine the dash's placement. This is obtained by taking the angular position of the terminator on the circle, and using the 'pos' function to convert it to Cartesian. In the degenerate case, we use the angular position of the next domain (minus the port width, converted to an angular distance).
        let ex,ey = if degenerate then
                      float i + 1.0 - (asin (port_width / c.radius) / a) |> pos
                    else
                      float i + overhang_dist_next |> pos
        let e1x, e1y = ex - l * cos ev, ey - l * sin ev
        let e2x, e2y = ex + l * cos ev, ey + l * sin ev
        let e = sprintf "<path class=\"%s\" d=\"M %f,%f L %f,%f\"/>" (dom_class next) e1x e1y e2x e2y
        // Output the SVG text combining both parts.
        sprintf "%s\n%s" arr e
      | Xing d ->
        // This is a crossing. It gets rendered as two parallel lines directed outwards from the circle, plus two arc segments to connect it to the rest of the circle. Note that, because both circles are going to draw this, there are going to be two overlapping paths for each side in the final SVG.
        let overhang_size_next = match next with Hole -> overhang_to_hole | _ -> 0.5
        let overhang_size_prev = match prev with Hole -> overhang_to_hole | _ -> 0.5
        // Figure out the positions of the attachment points of the straight lines. I'll need them in site index angular units for the arc function, and in Cartesian coordinates for the path statements.
        let dv = asin (port_width/ c.radius) / a
        let ax1, ay1 = float i - dv |> pos
        let ax2, ay2 = float i + dv |> pos
        let v = a * float i + c.angle + da
        // I'm extending the line a little bit in order to fake a miter join with the arc. The magic number here depends on the line thickness. A better solution would require rewriting this so that there is only one path declaration per side, instead of two.
        let side_length = 3.7 + xing_side_length b
        let bx1, by1 = ax1 + 0.5 * side_length * cos v, ay1 + 0.5 * side_length * sin v
        let bx2, by2 = ax2 + 0.5 * side_length * cos v, ay2 + 0.5 * side_length * sin v
        // Output the SVG text. Note that we are not adding the labels here. This is done further down, in the 'child' function.
        [ rim (float i - overhang_size_prev) (float i - dv) b
        ; rim (float i + dv) (float i + overhang_size_next) b
        ; sprintf "<path class=\"%s\" d=\"M %f,%f L %f,%f\"/>" (dom_class b) ax1 ay1 bx1 by1
        ; sprintf "<path class=\"%s\" d=\"M %f,%f L %f,%f\"/>" (dom_class b) ax2 ay2 bx2 by2 ]
          |> String.concat "\n"
      | b ->
        // This is a domain. It gets rendered as an arc.
        sprintf "%s\n%s" (arc i b) (label i b)
    /// Gathers all renderings of sites on the circle as SVG strings. Note that I am retrieving the previous and next port at each step; the changes I made to the renderer need knowledge of neighbors for some types of ports.
    let port_str = c.ports |> Array.mapi (fun i dom ->
        let prev = c.ports.[if i = 0 then c.ports.Length-1 else i-1]
        let next = c.ports.[if i = c.ports.Length-1 then 0 else i+1]
        port i prev dom next)
    /// This function draws a straight edge between two sites. I don't know in which cases this is supposed to happen, if any.
    let edge (i, j) =
      let x1, y1 = i |> float |> pos
      let x2, y2 = j |> float |> pos
      sprintf "<path class=\"circle_edge\" d=\"M %f,%f L %f,%f\"/>" x1 y1 x2 y2
    /// This would render any remaining circle edges (as straight lines) and gather them in a set of SVG strings, but from what I understand there should never be any circle edges at this stage. I couldn't find any case where this would happen.
    let edges_str = c.edges |> Array.map edge
    /// This function renders a child circle recursively. It requires the position (in site index units) of the attachment point, the domain that connects the circles, and the child circle itself. Note that at this stage the child circle is already centered at the correct distance from the origin (i.e. the parent).
    let child (i, b:svg_domain, ch) =
      /// The Cartesian coordinates of the attachment point position.
      let x, y = i |> float |> pos
      /// The angle of the attachment point position, in radians.
      let v = a * float i + c.angle + da
      /// The rotation of the label text, in degrees.
      let r = v * 360.0 / twopi
      let dx = xing_side_length b / 4.0
      // Add the labels for the connecting domain, then add the recursively generated circle (giving it the angular and Cartesian deltas required to transform it back into top-level space).
      [ sprintf "<text class=\"%s\" x=\"%f\" y=\"%f\" transform=\"rotate(%f %f,%f)\" dx=\"%f\" dy=\"-8\">%s</text>" (dom_class b) x y r x y dx b.text
      ; sprintf "<text class=\"%s\" x=\"%f\" y=\"%f\" transform=\"rotate(%f %f,%f)\" dx=\"-%f\" dy=\"-8\">%s</text>" (dom_class b) x y (r+180.0) x y dx b.compl.text
      ; convert (c.angle + da) (transform c.center) ch ]
        |> String.concat "\n"
    let children_str = c.children |> List.map child |> Array.ofList
    [| port_str; edges_str; children_str |] |> Array.concat |> String.concat "\n"
  // Recursively convert the circle, bracket it with an SVG element, add style, scale and size.
  c |> convert 0.0 (0.0, 0.0) |> sprintf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%f\" height=\"%f\">\n<style>%s</style>\n<g transform=\"scale(%f)\">\n%s\n</g>\n</svg>" (width*scale) (height*scale) style scale

// The following functions are used to convert a strand graph object into the initial circle.

type port = {strand:int;site:int}
type edge = (port*port)

/// Converts the edges to their representation in a global sequence made by arranging all strands one after the other. In this representation, an edge is an int couple where each int is the index of a site in the global sequence, and the two sites match. Returned edges are reverse-ordered by the position of their lowest-index site (e.g. [(9,12),(5,8),(1,4)]).
let get_edges (S:StrandC[]) (E:Set<edge>) = 
    /// The offsets array. This maps each strand (by index) to its starting position in a sequence of all sites, plus holes between strands (e.g. if S contains three strands, respectively made of 2, 3 and 2 sites, then the offsets array is [0,3,7]).
    let offsets = Array.init S.Length (fun i -> if i = 0 then 0 else i + (S.[0..i-1] |> Array.map(fun s -> s.Length) |> Array.sum))
        //S        
        //|> Seq.fold (fun (n,l) st -> (n+1, (n,1+st.Length)::l)) (1,[0,0])
        //|> snd
        //|> Map.ofSeq
    /// Maps a port (i.e. an int*int where the first item is the strand index and the second is the site index within the strand) to a port in the global sequence (i.e. an int).
    let port_map (a,b) = offsets.[a] + b
    /// Maps an edge (i.e. a couple of ports) to an edge in the global sequence (i.e. two ints that are the indexes of two matching sites in the global sequence).
    let edge_map (a,b) = port_map a, port_map b
  
    E
    |> Set.toArray
    |> Array.map(fun (p,p') -> (p.strand, p.site), (p'.strand, p'.site))
    |> Seq.map edge_map
    // I now have the edges as represented in the global sequence.
    |> Seq.map sort_e
    |> Seq.toArray
    // I now have the edges in the global sequence, normalized so that the lowest index site is always first.
    // I will reverse sort them on the first site.
    |> Array.sortWith (fun (a1,b1) (a2,b2) -> a2-a1) //ensure right-neighbours are taken first
    
/// Reorders the set of strands (with corresponding edges) according to the order supplied (the input order array must be a permutation of 0..S.Length-1). Returns the reordered set of strands, and the edges as represented in the global sequence.
let reorderBy (order:int[]) (S:StrandC[]) (E:Set<edge>) = 
    /// The order is a map from a strand's index in the old array, to the strand's index in the new array.
    /// The strands array, reordered according to the specified order.
    let S' = Array.zeroCreate S.Length
    Array.iteri (fun i v -> Array.set S' v S.[i]) order
    //let S' = [|0..S.Length-1|] |> Array.map(fun i -> S.[order.[i]])
    /// The edges, changed so that they refer to the correct strands in the reordered strands array.
    let E' = 
        E 
        |> Set.map(fun (p,p') ->
            {p  with site = p.site;  strand = order.[p.strand]},
            {p' with site = p'.site; strand = order.[p'.strand]})
    S', get_edges S' E'

/// Returns a permutation of the strands array that avoids pseudoknots. Returns the reordered set of strands, and the edges as represented in the global sequence.
let find_perm (S:StrandC[]) (E:Set<edge>) = 

    /// Takes an element and a list. Returns the set of all lists generated by inserting that element at each possible position within the list.
    let distrib e L =
        let rec aux pre post = 
            seq {
                match post with
                | [] -> yield (L @ [e])
                | h::t -> yield (List.rev pre @ [e] @ post)
                          yield! aux (h::pre) t 
            }
        aux [] L

    /// Takes a list of objects, returns a set of lists that are all possible permutations of the original list.
    let rec perms = function 
        | [] -> Seq.singleton []
        | h::t -> Seq.collect (distrib h) (perms t)

    /// Returns true if the edges would cross. We call this situation a pseudoknot. Pseudoknots cannot be rendered by this algorithm.
    let pseudoknot (e1,e1') (e2, e2') =         
        (e1>e2 && e1<e2' && (e1'>e2'||e1'<e2)) ||
        (e1'>e2 && e1'<e2' && (e1>e2'||e1<e2))

    /// Get all possible reorderings of the strands after the first (the first always remains the first).
    [1..S.Length-1]
    |> perms
    |> Seq.map(fun l -> 0::l |> List.toArray)
    |> Seq.map(fun order -> reorderBy order S E)
    // Now I have the set of all possible ways to reorder the strands; for each of these orderings, I also have the set of edges, expressed as couples of ints where each int is the index of a site in the sequence formed by arranging the strands one after the other in the specified order. I search this set for an ordering that has no pseudoknots.
    |> Seq.tryFind(fun (_, E') -> 
        E'|> Seq.forall (fun e ->             
                    E' |> Seq.forall(fun e' -> not (pseudoknot e e' || pseudoknot e' e))))
        
/// Returns the circle made of all the domains in all the strands in the given species. This circle will then require collapsing.
let make_base_circle (ports:svg_domain[]) (edges:(int*int)[]) =
  { center   = (0.0, 0.0)
  ; angle    = 0.0
  ; radius   = ports |> circle_radius
  ; ports    = ports
  ; children = []
  ; edges    = edges
  }

/// Converts a species in an uncollapsed circle representation. Requires the strands and the species (only the strand graph of the species is actually used).
let from_logic_dsd (l:RulesDSD.Syntax.Process<Microsoft.Research.DNA.LogicDSD.SiteT>) : circle =
  let c = Microsoft.Research.DNA.LogicDSD.to_concrete_process l
  
  /// Most of the algorithms here operate on the assumption that bonds are represented as edges, which are couples of ports (where a port is a position in a strand). In a ProcessC object, there isn't the notion of a port; rather, each site can have a bond ID associated to it. This function converts from the bond ID representation to the edges representation.
  let map_strand_bonds_to_edges (existing:Map<int,edge>) (strand_idx:int,strand:StrandC) =
    let map_site_bond_to_edge (existing:Map<int,edge>) (site_idx:int,site:SiteC) =
      match site with
      | UnboundC _ -> existing
      | BoundC (_,b:int) ->
        match Map.tryFind b existing with
        | None ->
          Map.add b ({strand=strand_idx;site=site_idx},{strand=0;site=0}) existing
        | Some (from,_) ->
          Map.add b (from,{strand=strand_idx;site=site_idx}) existing
    List.mapi (fun i site -> i,site) strand |> List.fold map_site_bond_to_edge existing
  let bonds_as_edges = List.mapi (fun i strand -> i,strand) c |> List.fold map_strand_bonds_to_edges Map.empty

  /// The strands and edges, rearranged so that there are no pseudoknots (if possible). The edges will be remapped so that their sites are indexes in the global sequence (all the strands one after the other, with holes between strands).
  let S, E =
    let SS = c |> List.toArray
    let EE = bonds_as_edges |> Map.toSeq |> Seq.map (fun (_,v) -> v) |> Set.ofSeq
    match find_perm SS EE with
    | Some (a,b) -> a,b
    | None -> failwith "Cannot visualize pseudoknot"
 
  /// The global sequence. This is a sequence of sites (in the form of svg_domain objects) which corresponds to all the strands aligned one after the other, in the order determined above. A 'hole' domain is inserted after each strand, making the total length of the sequence equal to the sum of all domains, plus number of strands.
  let ports =
    S
    |> Array.fold (fun acc st ->
            let d =
                st
                |> List.map (fun s -> match s with | BoundC (d,_) -> d | UnboundC d -> d)
                |> List.map (fun d -> Domain d)
                |> List.rev
            Hole::d@acc) List.empty
    |> List.rev  
    |> List.toArray
  
  make_base_circle ports E

let from_domains (doms:Domain.t list) =
  let map_domain (dom:Domain.t) =
    { name = Domain.get_name (Domain.unstar dom)
      isComplementary = Domain.is_complemented dom
      isToehold = Domain.is_toehold dom
      tag = NoTagC }
    |> Domain
  List.map map_domain doms

let from_strand (strand:Strand.t) =
  let domains = match strand with
                | Strand.Upper s -> from_domains s
                | Strand.Lower s -> from_domains s |> List.rev
  let domains = Hole::domains
  make_base_circle (Array.ofList domains) [||]

/// Converts a Classic DSD segment in a circle.
let from_segment (segment:Segment.t) =
  match segment with
  | Segment.Double (lb,lt,s,rt,rb) ->
    let top = from_domains (lt @ s @ rt)
    let bottom = from_domains (lb @ (List.map (fun d -> Domain.complement d) s) @ rb) |> List.rev
    let domains = Hole::top@Hole::bottom
    let edges = [1..s.Length] |> List.map (fun i -> (i+lt.Length,domains.Length-i-lb.Length))
    make_base_circle (Array.ofList domains) (Array.ofList edges)
  | Segment.Hairpin (ob,ot,s,hp,side) ->
    match side with
    | Segment.Left ->
      let domains = from_domains ((List.rev ob) @ (List.map (fun d -> Domain.complement d) s |> List.rev) @ (List.rev hp) @ s @ ot)
      let domains = Hole::domains
      let edges = [1..s.Length] |> List.map (fun i -> (i+ob.Length,domains.Length-i-ot.Length))
      make_base_circle (Array.ofList domains) (Array.ofList edges)
    | Segment.Right ->
      let domains = from_domains (ot @ s @ (List.rev hp) @ (List.map (fun d -> Domain.complement d) s |> List.rev) @ (List.rev ob))
      let domains = Hole::domains
      let edges = [1..s.Length] |> List.map (fun i -> (i+ot.Length,domains.Length-i-ob.Length))
      make_base_circle (Array.ofList domains) (Array.ofList edges)

let from_gate (gate:Gate.t) =
  let lower_join_circles (circles:circle list) : circle =
    /// Bottom-fuses two circles. The last strand of each circle is bottom, and edges are in the form (top,bottom).
    let fuse (left:circle) (right:circle) : circle =
      let left_fuse_point = Array.findIndexBack (fun (d:svg_domain) -> d = svg_domain.Hole) left.ports
      let (left_top, left_bottom) = List.splitAt left_fuse_point (left.ports |> List.ofArray)
      let right_fuse_point = Array.findIndexBack (fun (d:svg_domain) -> d = svg_domain.Hole) right.ports
      let (right_top, right_bottom) = List.splitAt right_fuse_point (right.ports |> List.ofArray)
      let bottom = right_bottom@(left_bottom.Tail)
      let domains = left_top @ right_top @ bottom
      // The indexes within edges in the left circle are shifted if they are after the fuse point.
      let left_edges =
        let shift = right_top.Length+right_bottom.Length-1
        left.edges |> Array.map (fun (top,bottom) -> (if top < left_fuse_point then top else top+shift),(if bottom < left_fuse_point then bottom else bottom+shift))
      // The right circle always remains contiguous, so edges from the right circle are always shifted.
      let right_edges =
        let shift = left_top.Length
        right.edges |> Array.map (fun (top,bottom) -> (top+shift,bottom+shift))
      let edges = Array.concat [left_edges;right_edges]
      make_base_circle (Array.ofList domains) edges
    List.fold fuse circles.Head circles.Tail
  let upper_join_circles (circles:circle list) : circle =
    /// Top-fuses two circles. The last strand of each circle is bottom.
    let fuse (left:circle) (right:circle) : circle =
      let left_fuse_point = Array.findIndexBack (fun (d:svg_domain) -> d = svg_domain.Hole) left.ports
      let (left_top, left_bottom) = List.splitAt left_fuse_point (left.ports |> List.ofArray)
      let right_all = List.ofArray right.ports
      let domains = left_bottom @ left_top @ (right_all.Tail)
      // The indexes within edges in the left circle are shifted forward if they are before the fuse point, and shifted backwards if they are after the fuse point.
      let left_edges = left.edges |> Array.map (fun (top,bottom) -> (if top < left_fuse_point then (top+left_bottom.Length) else (top-left_top.Length)),(if bottom < left_fuse_point then (bottom+left_bottom.Length) else (bottom-left_top.Length)))
      // The right circle always remains continguous, so edges from the right circle are always shifted.
      let right_edges =
        let shift = left.ports.Length-1
        right.edges |> Array.map (fun (top,bottom) -> (top+shift,bottom+shift))
      let edges = Array.concat [left_edges;right_edges]
      make_base_circle (Array.ofList domains) edges      
    List.fold fuse circles.Head circles.Tail
  List.map (fun segments -> segments |> List.map from_segment |> lower_join_circles) gate |> upper_join_circles

let arrange_origami style (origami:Origami.t) =
  let origami_sep = { Microsoft.Research.CRNEngine.Svg.x = 16.0; Microsoft.Research.CRNEngine.Svg.y = 16.0 }
  let species_skip = 10.0
  let letter_width = 8.0
  let origami_round = 32.0
  let parts = origami |> List.map (fun el -> match el with Origami.C_strand s -> from_strand s | Origami.C_gate g -> from_gate g) |> List.map collapse_circle |> List.map into_view
  let to_box (circle,width,height) : Microsoft.Research.CRNEngine.Svg.box =
    { box_group = { name = ""
                  ; content = circle_to_svg "" 1.0 (circle,width,height) |> Microsoft.Research.CRNEngine.Svg.Raw
                  ; anchors = []
                  ; debug_anchors = false
                  ; sub_groups = []
                  ; offset = None
                  }
    ; box_dim = { x = width; y = height } }
  let b = parts |> List.map to_box |> Microsoft.Research.CRNEngine.Svg.stackv origami_sep species_skip
  let border_label = "" in
  let border_inset = (String.length border_label |> float) * letter_width / 2.0 in
  let border_offset = { Microsoft.Research.CRNEngine.Svg.x = 2.0; Microsoft.Research.CRNEngine.Svg.y = 2.0 } in
  let border_dim = b.box_dim |> Microsoft.Research.CRNEngine.Svg.translate_point_by (border_offset |> Microsoft.Research.CRNEngine.Svg.scale_by -1.0) in
  let border = { Microsoft.Research.CRNEngine.Svg.path_class = "svgdsd origami-border" |> Some
               ; Microsoft.Research.CRNEngine.Svg.commands = Microsoft.Research.CRNEngine.Svg.rounded_box border_offset border_dim origami_round
               ; Microsoft.Research.CRNEngine.Svg.path_label = { Microsoft.Research.CRNEngine.Svg.label_text = border_label
                                                               ; Microsoft.Research.CRNEngine.Svg.label_anchor = Microsoft.Research.CRNEngine.Svg.sub b.box_dim origami_sep |> Microsoft.Research.CRNEngine.Svg.translate_point_by { x = -border_inset; y = 0.0 }
                                                               ; Microsoft.Research.CRNEngine.Svg.label_class = Some "svgdsd"
                                                               ; Microsoft.Research.CRNEngine.Svg.letter_width = Microsoft.Research.CRNEngine.Svg.default_letter_width
                                                               ; Microsoft.Research.CRNEngine.Svg.label_dir = Microsoft.Research.CRNEngine.Svg.sub b.box_dim origami_sep |> Microsoft.Research.CRNEngine.Svg.translate_point_by { x = -border_inset; y = 0.0 } }
                                                               |> Some }
  let g = {b.box_group with content = Paths [border]}
  let box = Microsoft.Research.CRNEngine.Svg.emerge_groups [g]
  Microsoft.Research.CRNEngine.Svg.to_string_normalise false style box

let species_to_svg_circles style (proc:Species.t) =
  match proc with
  | UNKNOWN unknown ->
    let g = { Microsoft.Research.CRNEngine.Svg.name = unknown
              Microsoft.Research.CRNEngine.Svg.content = Microsoft.Research.CRNEngine.Svg.Raw unknown
              Microsoft.Research.CRNEngine.Svg.anchors = []
              Microsoft.Research.CRNEngine.Svg.debug_anchors = false 
              Microsoft.Research.CRNEngine.Svg.sub_groups = [] 
              Microsoft.Research.CRNEngine.Svg.offset = None }
    let box = Microsoft.Research.CRNEngine.Svg.emerge_groups [g]
    Microsoft.Research.CRNEngine.Svg.to_string_normalise true style box
  | STRAND strand ->
    from_strand strand |> collapse_circle |> into_view |> circle_to_svg style 1.0
  | GATE gate ->
    from_gate gate |> collapse_circle |> into_view |> circle_to_svg style 1.0
  | ORIGAMI origami ->
    arrange_origami style origami
  | LogicDsdProcess proc ->
    from_logic_dsd proc |> collapse_circle |> into_view |> circle_to_svg style 1.0
