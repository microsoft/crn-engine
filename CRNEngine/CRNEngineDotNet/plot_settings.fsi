[<JavaScript>]
module Microsoft.Research.CRNEngine.Plot_settings 
open WebSharper

type t = {
  x_label: string;
  y_label: string;
  title: string;
  label_font_size: int;
  tick_font_size: int;
  x_ticks: float list;
  y_ticks: float list;
}

val defaults: t;

val parse : Parser.t<t>;