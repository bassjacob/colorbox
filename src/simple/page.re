type canvasRenderingContext;
type canvasElement;

external window: Dom.window = "" [@@bs.val];
external document: Dom.document = "" [@@bs.val];

external elementToCanvasElement : Dom.element => canvasElement = "%identity";

external createElement : Dom.document => string => Dom.element = "" [@@bs.send];

external getContext : canvasElement => string => canvasRenderingContext = "" [@@bs.send];

external setWidth : canvasElement => float => unit = "width" [@@bs.set];
external setHeight : canvasElement => float => unit = "height" [@@bs.set];
external getClientWidth : Dom.element => float = "clientWidth" [@@bs.get];
external getClientHeight : Dom.element => float = "clientHeight" [@@bs.get];

external drawImage : canvasRenderingContext => Dom.element => float => float => float => float => unit = "" [@@bs.send];

type imageData = Js.t {
  .
  data: array float
};

external getImageData : canvasRenderingContext => float => float => float => float => imageData = "" [@@bs.send];

type rgba = RGBA int int int int | RGBAF float float float float;
type background = {
  top: rgba,
  bottom: rgba
};
type text = rgba;
type state = {
  background: background,
  text: text
};

let rgbaToString (RGBA r g b a) => {j| rgba($r, $g, $b, $a) |j};

let createCanvas () => elementToCanvasElement @@ createElement document "canvas";

let cv img => {
  let calculateTextColor (RGBA r g b a) => {
    let v = (1.0 -. ( 0.299 *. (float r) +. 0.587 *. (float g) +. 0.114 *. (float b)) /. 255.0) < 0.5 ? 0 : 255;
    RGBA v v v 1;
  };

  let calculateAverageRGBValues data => {
    let fval len x => Js.Math.floor (x /. (len /. 4.0));
    let arr = Array.mapi (fun i a => (i, a)) data;
    let munge = fun (i, v) (RGBAF r g b a) => {
      switch (i mod 4) {
      | 0 => RGBAF (r +. v) g b a
      | 1 => RGBAF r (g +. v) b a
      | 2 => RGBAF r g (b +. v) a
      | 3 => RGBAF r g b (a +. v)
      };
    };
    let (RGBAF r g b a) = Array.fold_right munge arr (RGBAF 0.0 0.0 0.0 0.0);
    let f = fval (float @@ Array.length data);
    RGBA (f r) (f g) (f b) (f a);
  };

  let imgWidth = getClientWidth img;
  let imgHeight = getClientHeight img;

  let canvas = createCanvas ();
  let context = getContext canvas "2d";
  setWidth canvas imgWidth;
  setHeight canvas imgHeight;

  drawImage context img 0.0 0.0 imgWidth imgHeight;
  let top = calculateAverageRGBValues (getImageData context 0.0 0.0 imgWidth 30.0)##data;
  let bottom = calculateAverageRGBValues (getImageData context 0.0 (imgHeight -. 30.0) imgWidth 30.0)##data;
  let text = calculateTextColor top;

  {
    background: {
      top: top,
      bottom: bottom
    },
    text: text
  };
};

module RawImage = {
  external setOnload : Dom.element => ('a => unit) => unit = "onload" [@@bs.set];
  let s update elm => switch (Js.Null.to_opt elm) {
  | Some e => setOnload e (update (fun _ state _ => { ReasonReact.Update (cv e); }))
  | None => ()
  };

  let component = ReasonReact.statefulComponent "RawImage";

  let make ::imgPath _children => {
    {
      ...component,
      initialState: fun () => {
        background: {
          top: RGBA 255 255 255 1,
          bottom: RGBA 255 255 255 1
        },
        text: RGBA 0 0 0 1
      },
      render: fun (state:state) {update} => {
        let top = rgbaToString state.background.top;
        let bottom = rgbaToString state.background.bottom;
        let rawImageStyle = ReactDOMRe.Style.make
          display::"flex"
          width::"100%"
          alignItems::"center"
          justifyContent::"center"
          transition::"opacity 2s"
          background::({j| linear-gradient(to bottom, $top, $bottom) |j})
          ();
        let textStyle = ReactDOMRe.Style.make
          textAlign::"center"
          padding::"5%"
          transition::"color 1s"
          color::(rgbaToString state.text)
          ();
        <div style=rawImageStyle>
          <div style=textStyle>
            <h1> (ReasonReact.stringToElement "Text") </h1>
            <img
              ref=(s update)
              src=imgPath
            />
          </div>
        </div>
      }
    }
  };
};

let component = ReasonReact.statelessComponent "Greeting";

let make ::name _children => {
  {
    ...component,
    render: fun _state _self => {
      <div>
        <RawImage imgPath="/public/images/1.jpg" />
        <RawImage imgPath="/public/images/2.jpg" />
        <RawImage imgPath="/public/images/3.jpg" />
        <RawImage imgPath="/public/images/4.jpg" />
        <RawImage imgPath="/public/images/5.jpg" />
        <RawImage imgPath="/public/images/6.jpg" />
        <RawImage imgPath="/public/images/rhino.jpg" />
      </div>
    }
  }
};
