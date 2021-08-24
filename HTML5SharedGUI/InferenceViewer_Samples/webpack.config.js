/// <binding AfterBuild='Run - Development' />
"use strict";
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CleanWebpackPlugin = require('clean-webpack-plugin');
var path = require('path');
module.exports = [{
    mode: "development",
    entry: {
        'DensityPlot': "./DensityPlot/App.js",
        'ModelDataDynamics': "./ModelDataDynamics/App.js",
        'ParametersViewer': "./ParametersViewer/App.js",
        'PosteriorTable': "./PosteriorTable/App.js",
        'ScatterPlot': "./ScatterPlot/App.js",
        'Summary': "./Summary/App.js"
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
    module: {
        rules: [
            {
                // https://github.com/webpack/webpack/issues/1406
                // Possible long-term fix: switch from KO to TKO.
                test: require.resolve('jquery'),
                use: [{
                    loader: 'expose-loader',
                    options: 'jQuery'
                }]
            },
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
            template: './DensityPlot/index.html',
            excludeChunks: ['ModelDataDynamics', 'ParametersViewer', 'PosteriorTable', 'ScatterPlot', 'Summary'],
            filename: './DensityPlot.html'
        }),
        new HtmlWebpackPlugin({
            template: './ModelDataDynamics/index.html',
            excludeChunks: ['DensityPlot', 'ParametersViewer', 'PosteriorTable', 'ScatterPlot', 'Summary'],
            filename: './ModelDataDynamics.html'
        }),
        new HtmlWebpackPlugin({
            template: './ParametersViewer/index.html',
            excludeChunks: ['DensityPlot', 'ModelDataDynamics', 'PosteriorTable', 'ScatterPlot', 'Summary'],
            filename: './ParametersViewer.html'
        }),
        new HtmlWebpackPlugin({
            template: './PosteriorTable/index.html',
            excludeChunks: ['DensityPlot', 'ModelDataDynamics', 'ParametersViewer', 'ScatterPlot', 'Summary'],
            filename: './PosteriorTable.html'
        }),
        new HtmlWebpackPlugin({
            template: './ScatterPlot/index.html',
            excludeChunks: ['DensityPlot', 'ModelDataDynamics', 'ParametersViewer', 'PosteriorTable', 'Summary'],
            filename: './ScatterPlot.html'
        }),
        new HtmlWebpackPlugin({
            template: './Summary/index.html',
            excludeChunks: ['DensityPlot', 'ModelDataDynamics', 'ParametersViewer', 'PosteriorTable', 'ScatterPlot'],
            filename: './Summary.html'
        })
    ]
}];