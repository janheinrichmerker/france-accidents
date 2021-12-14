const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const RemoteFilePlugin = require('remote-file-webpack-plugin');

module.exports = {
    mode: "development",
    entry: "./src/index.js",
    plugins: [
        new HtmlWebpackPlugin({
            template: "./src/index.html",
        }),
        new RemoteFilePlugin([
            {
                url: "https://www.data.gouv.fr/en/datasets/r/07a88205-83c1-4123-a993-cba5331e8ae0",
                filepath: "data/characteristics-2020.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/e22ba475-45a3-46ac-a0f7-9ca9ed1e283a",
                filepath: "data/characteristics-2019.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/6eee0852-cbd7-447e-bd70-37c433029405",
                filepath: "data/characteristics-2018.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/9a7d408b-dd72-4959-ae7d-c854ec505354",
                filepath: "data/characteristics-2017.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/96aadc9f-0b55-4e9a-a70e-c627ed97e6f7",
                filepath: "data/characteristics-2016.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/185fbdc7-d4c5-4522-888e-ac9550718f71",
                filepath: "data/characteristics-2015.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/85dfe8c6-589f-4e76-8a07-9f59e49ec10d",
                filepath: "data/characteristics-2014.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/18b1a57a-57bf-4bf1-b9ee-dfa5a3154225",
                filepath: "data/characteristics-2013.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/b2518ec1-6529-47bc-9d55-40e2effeb0e7",
                filepath: "data/characteristics-2012.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/37991267-8a15-4a9d-9b1c-ff3e6bea3625",
                filepath: "data/characteristics-2011.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/decdfe8c-38ff-4a06-b7fc-615785f2914d",
                filepath: "data/characteristics-2010.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/fdfacdb9-f48e-4759-bae5-48d063216acb",
                filepath: "data/characteristics-2009.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/722ebb99-c8b2-4635-bf8d-125dd280ee42",
                filepath: "data/characteristics-2008.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/6fc7b169-4dfe-442c-8c28-8bd773aeddf8",
                filepath: "data/characteristics-2007.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/fafa33cf-50cb-4092-a819-d5209f684089",
                filepath: "data/characteristics-2006.csv",
                cache: true,
            },
            {
                url: "https://www.data.gouv.fr/en/datasets/r/a47866f7-ece1-4de8-8d31-3a1b4f477e08",
                filepath: "data/characteristics-2005.csv",
                cache: true,
            },
        ]),
        new CopyWebpackPlugin({
            patterns: [
                {from: "./static"},
            ],
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
