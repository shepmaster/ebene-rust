const path = require("path");

const HtmlWebpackPlugin = require('html-webpack-plugin');
const HtmlWebpackTemplate = require('html-webpack-template');

module.exports = {
  plugins: [
    new HtmlWebpackPlugin({
      template: HtmlWebpackTemplate,
      title: 'Strata Rust',
      appMountId: 'app',
      mobile: true,
      inject: false,
    }),
  ],
  resolve: {
    alias: {
      app: path.resolve(__dirname, "app"),
    },
    extensions: ['.js', '.jsx', '.ts', '.tsx'],
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        loader: "awesome-typescript-loader",
      },
      {
        enforce: "pre",
        test: /\.js$/,
        loader: "source-map-loader",
      },
      {
        test: /\.jsx?$/,
        loader: 'babel-loader',
        options: {
          // Improved performance during development.
          cacheDirectory: true,
        },
      },
      {
        test: /\.scss$/,
        loaders: ["style-loader", "css-loader", "sass-loader"]
      },
      {
        test: /\.css$/,
        loaders: ["style-loader", "css-loader"]
      },
    ],
  },
  entry: ['babel-polyfill', './app/index.tsx'],
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, './dist'),
  },
  devServer: {
    proxy: {
      '/api': {
        target: 'http://127.0.0.1:8000/',
        secure: false,
        pathRewrite: {'^/api' : ''},
      }
    }
  }
};
