"use strict";
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CleanWebpackPlugin = require('clean-webpack-plugin');
var path = require('path');
module.exports = [{
    mode: "development",
    entry: {
        'Exports': "./Exports/app.js",
        'FastTasks': "./FastTasks/app.js",
        'Inference': "./Inference/app.js",
        'Parser': "./Parser/app.js",
        'Probabilities': "./Probabilities/app.js",
        'Serialized': "./Serialized/app.js",
        'Simulator': "./Simulator/app.js",
        'StateSpace': "./StateSpace/app.js"
    },
    output: {
        filename: "./[name].[hash].js",
        path: path.resolve(__dirname, 'dist')
    },
    optimization: {
        splitChunks: {
            chunks: 'all'
        }
    },
    resolve: {
        alias: {
            idd$: path.resolve(__dirname, '../../node_modules/interactive-data-display/dist/idd_knockout.js'),
            svg$: path.resolve(__dirname, '../../node_modules/svgjs'),
            filesaver$: path.resolve(__dirname, '../../node_modules/file-saver'),
            jquery$: path.resolve(__dirname, '../../node_modules/jquery')
        }
    },
    node: {
        fs: 'empty'
    },
    module: {
        rules: [
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            },
            {
                test: /\.(svg|png)(\?v=\d+\.\d+\.\d+)?$/,
                use: [{
                    loader: 'file-loader',
                    options: {
                        name: '[name].[ext]',
                        outputPath: 'img/'
                    }
                }]
            },
            {
                test: /\.wasm$/,
                type: "javascript/auto",
                loader: "file-loader"
            }
        ]
    },
    plugins: [
        new CleanWebpackPlugin(['dist']),
        new webpack.ProvidePlugin({
            $: 'jquery',
            jQuery: 'jquery',
            Rx: 'rx',
            ko: 'knockout',
            SVG: 'svgjs',
            saveAs: 'file-saver',
            jQuery_mousewheel: 'jquery-mousewheel'
        }),
        new HtmlWebpackPlugin({
            template: './Exports/index.html',
            excludeChunks: ['FastTasks', 'Inference', 'Parser', 'Probabilities', 'Serialized', 'Simulator', 'StateSpace'],
            filename: './Exports.html'
        }),
        new HtmlWebpackPlugin({
            template: './FastTasks/index.html',
            excludeChunks: ['Exports', 'Inference', 'Parser', 'Probabilities', 'Serialized', 'Simulator', 'StateSpace'],
            filename: './FastTasks.html'
        }),
        new HtmlWebpackPlugin({
            template: './Inference/index.html',
            excludeChunks: ['Exports', 'FastTasks', 'Parser', 'Probabilities', 'Serialized', 'Simulator', 'StateSpace'],
            filename: './Inference.html'
        }),
        new HtmlWebpackPlugin({
            template: './Parser/index.html',
            excludeChunks: ['Exports', 'FastTasks', 'Inference', 'Probabilities', 'Serialized', 'Simulator', 'StateSpace'],
            filename: './Parser.html'
        }),
        new HtmlWebpackPlugin({
            template: './Probabilities/index.html',
            excludeChunks: ['Exports', 'FastTasks', 'Inference', 'Parser', 'Serialized', 'Simulator', 'StateSpace'],
            filename: './Exports.html'
        }),
        new HtmlWebpackPlugin({
            template: './Serialized/index.html',
            excludeChunks: ['Exports', 'FastTasks', 'Inference', 'Parser', 'Probabilities', 'Simulator', 'StateSpace'],
            filename: './Serialized.html'
        }),
        new HtmlWebpackPlugin({
            template: './Simulator/index.html',
            excludeChunks: ['Exports', 'FastTasks', 'Inference', 'Parser', 'Probabilities', 'Serialized', 'StateSpace'],
            filename: './Simulator.html'
        }),
        new HtmlWebpackPlugin({
            template: './StateSpace/index.html',
            excludeChunks: ['Exports', 'FastTasks', 'Inference', 'Parser', 'Probabilities', 'Serialized', 'Simulator'],
            filename: './StateSpace.html'
        }),
    ]
}];