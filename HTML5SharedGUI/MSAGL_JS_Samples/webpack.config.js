/// <binding AfterBuild='Run - Development' />
"use strict";
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CleanWebpackPlugin = require('clean-webpack-plugin');
var WorkboxPlugin = require('workbox-webpack-plugin');
var path = require('path');
module.exports = [{
    mode: "development",
    entry: {
        'BigGraph': "./Samples/BigGraph/app.js",
        'Clusters': "./Samples/Clusters/app.js",
        'CustomLabels': "./Samples/CustomLabels/app.js",
        'GraphFromJson': "./Samples/GraphFromJson/app.js",
        'HiddenRendering': "./Samples/HiddenRendering/app.js",
        'Interactivity': "./Samples/Interactivity/app.js",
        'Options': "./Samples/Options/app.js",
        'ZoomControls': "./Samples/ZoomControls/app.js"
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
            //'interactive-data-display$': path.resolve(__dirname, '../../node_modules/interactive-data-display/dist/idd_knockout.js'),
            //svg$: path.resolve(__dirname, '../../node_modules/svgjs'),
            //filesaver$: path.resolve(__dirname, '../../node_modules/file-saver')
        }
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
            template: './Samples/BigGraph/index.html',
            excludeChunks: ['CustomLabels', 'Clusters', 'GraphFromJson', 'HiddenRendering', 'Interactivity', 'Options', 'ZoomControls'],
            filename: './BigGraph.html'
        }),
        new HtmlWebpackPlugin({
            template: './Samples/Clusters/index.html',
            excludeChunks: ['BigGraph', 'CustomLabels', 'GraphFromJson', 'HiddenRendering', 'Interactivity', 'Options', 'ZoomControls'],
            filename: './Clusters.html'
        }),
        new HtmlWebpackPlugin({
            template: './Samples/CustomLabels/index.html',
            excludeChunks: ['BigGraph', 'Clusters', 'GraphFromJson', 'HiddenRendering', 'Interactivity', 'Options', 'ZoomControls'],
            filename: './CustomLabels.html'
        }),
        new HtmlWebpackPlugin({
            template: './Samples/GraphFromJson/index.html',
            excludeChunks: ['BigGraph', 'Clusters', 'CustomLabels', 'HiddenRendering', 'Interactivity', 'Options', 'ZoomControls'],
            filename: './GraphFromJson.html'
        }),
        new HtmlWebpackPlugin({
            template: './Samples/HiddenRendering/index.html',
            excludeChunks: ['BigGraph', 'Clusters', 'CustomLabels', 'GraphFromJson', 'Interactivity', 'Options', 'ZoomControls'],
            filename: './HiddenRendering.html'
        }),
        new HtmlWebpackPlugin({
            template: './Samples/Interactivity/index.html',
            excludeChunks: ['BigGraph', 'Clusters', 'CustomLabels', 'GraphFromJson', 'HiddenRendering', 'Options', 'ZoomControls'],
            filename: './Interactivity.html'
        }),
        new HtmlWebpackPlugin({
            template: './Samples/Options/index.html',
            excludeChunks: ['BigGraph', 'Clusters', 'CustomLabels', 'GraphFromJson', 'HiddenRendering', 'Interactivity', 'ZoomControls'],
            filename: './Options.html'
        }),
        new HtmlWebpackPlugin({
            template: './Samples/ZoomControls/index.html',
            excludeChunks: ['BigGraph', 'Clusters', 'CustomLabels', 'GraphFromJson', 'HiddenRendering', 'Interactivity', 'Options'],
            filename: './ZoomControls.html'
        })
    ]
}];