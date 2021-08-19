// Webpack is executed by a custom target in the .csproj file. Webpack output is collected for packaging and deployment by another custom target.
"use strict";
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CleanWebpackPlugin = require('clean-webpack-plugin');
var WorkboxPlugin = require('workbox-webpack-plugin');
var path = require('path');
var MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = env => {
    if (env === undefined || env === null)
        env = {};
    if (env.mode === undefined || env === null)
        env.mode = "development";
    console.log("Mode is set to " + env.mode);
    return {
        mode: env.mode,
        devtool: "source-map",
        entry: {
            app: "./Scripts/app.js"
        },
        output: {
            filename: "./[name].[chunkhash].js",
            path: path.resolve(__dirname, 'dist')
        },
        optimization: {
            splitChunks: {
                chunks: 'all'
            }
        },
        // Suppress warnings about large file sizes. This is a complex app and we have a lot of code. That's why we have a loading screen.
        performance: { hints: false },
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
                    test: /\.(woff(2)?|ttf|eot)(\?v=\d+\.\d+\.\d+)?$/,
                    use: [{
                        loader: 'file-loader',
                        options: {
                            name: '[name].[ext]',
                            outputPath: 'fonts/'
                        }
                    }]
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
            new CopyWebpackPlugin([{
                from: 'Examples/**',
                to: '.',
                context: '../',
                flatten: false
            }, {
                from: '../../HTML5SharedGUI/GenericComponents/Styles/shared.css',
                to: 'shared.css',
                flatten: true
            }, {
                from: '../../HTML5SharedGUI/SimulationViewer/Img/refresh_icon.png',
                to: 'img/refresh_icon.png',
                flatten: true
            }, {
                from: './favicon.ico',
                to: 'favicon.ico',
                flatten: true
            }, {
                from: './web.config',
                to: '.',
                flatten: true
            }]),
            new webpack.ProvidePlugin({
                $: 'jquery',
                jQuery: 'jquery',
                Rx: 'rx',
                ko: 'knockout',
                SVG: 'svgjs',
                saveAs: 'file-saver',
                jQuery_mousewheel: 'jquery-mousewheel'
            }),
            new MonacoWebpackPlugin({ languages: [] }),
            new HtmlWebpackPlugin({
                template: './Templates/index.html',
                filename: './index.html' //relative to root of the application
            }),
            new WorkboxPlugin.GenerateSW({
                swDest: './service-worker.js',
                clientsClaim: true,
                skipWaiting: true,
                // We have quite a lot of code, but we still want to precache it.
                maximumFileSizeToCacheInBytes: 5 * 1024 * 1024
            })
        ]
    };
};