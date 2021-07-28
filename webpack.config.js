const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
  mode: "development",
  entry: "./src/index.js",
  plugins: [
    new HtmlWebpackPlugin({
      template: "./src/index.html",
    }),
  ],
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "[name].js",
  },
  module: {
    rules: [
      {
        test: /\.elm$/i,
        exclude: [/elm-stuff/, /node_modules/],
        use: ["elm-hot-webpack-loader", "elm-webpack-loader"],
      },
    ],
    noParse: /\.elm$/,
  },
};
