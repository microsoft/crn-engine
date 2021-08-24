declare module "*.html"
declare module "idd" {
    var InteractiveDataDisplay: any;
    export = InteractiveDataDisplay;
}
declare module "tinycolor" {
    var tinycolor: any;
    export = tinycolor;
}
declare module "worker-loader*" {
    class WebpackWorker extends Worker {
        constructor();
    }
    export default WebpackWorker;
}