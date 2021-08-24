/// <binding AfterBuild='Run - Development' />
"use strict";
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CleanWebpackPlugin = require('clean-webpack-plugin');
var path = require('path');
module.exports = [{
    mode: "development",
    entry: {
        'CRNEditorApp': "./CRNEditorApp.js",
        'CRNEditorErrorsApp': "./CRNEditorErrorsApp.js"
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
            template: './CRNEditor.html',
            excludeChunks: ['CRNEditorErrorsApp'],
            filename: './CRNEditor.html'
        }),
        new HtmlWebpackPlugin({
            template: './CRNEditorErrors.html',
            excludeChunk: ['CRNEditorApp'],
            filename: './CRNEditorErrors.html'
        })
    ]
}];