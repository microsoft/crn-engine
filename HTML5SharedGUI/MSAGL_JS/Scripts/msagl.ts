import Worker from "worker-loader?filename=./msagl.worker.[hash].js!./msagl.worker.js";
import { SetWorkerConstructor as SetWorkerConstructor } from "./ggraph"
SetWorkerConstructor(Worker);

export * from "./ggraph"
export { default as ContextGraph } from "./contextgraph"
export { default as IDDGraph } from "./iddgraph"
export { default as SVGGraph } from "./svggraph"
export { default as IDDSVGGraph } from "./iddsvggraph"