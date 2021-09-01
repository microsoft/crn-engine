// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');

module.exports = function override(config, env) {
  if (!config.plugins) {
    config.plugins = [];
  }
  config.plugins.push(new MonacoWebpackPlugin({
    languages: ['json']
  }));
  return config;
}
