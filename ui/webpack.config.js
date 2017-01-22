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
    extensions: ['.js', '.jsx'],
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        //      include: paths,
        loader: 'babel-loader',
        options: {
          // Improved performance during development.
          cacheDirectory: true,
        },
      },
      {
        test: /\.css$/,
        // include: paths,
        use: ['style-loader', 'css-loader'],
      },
    ],
  },
  entry: './app/index.jsx',
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
