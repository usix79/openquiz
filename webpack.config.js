// Template for webpack.config.js in Fable projects
// Find latest version in https://github.com/fable-compiler/webpack-config-template

// In most cases, you'll only need to edit the CONFIG object (after dependencies)
// See below if you need better fine-tuning of Webpack options

// Dependencies. Also required: core-js, fable-loader, fable-compiler, @babel/core,
// @babel/preset-env, babel-loader, sass, sass-loader, css-loader, style-loader, file-loader, resolve-url-loader
var path = require('path');
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');
var MiniCssExtractPlugin = require('mini-css-extract-plugin');
var BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;

var CONFIG = {
    // The tags to include the generated JS and CSS will be automatically injected in the HTML template
    // See https://github.com/jantimon/html-webpack-plugin
    indexHtmlTemplate: './src/client/index.html',
    fsharpEntry: './src/client/App.fs.js',
    cssEntry: './src/client/style.scss',
    outputDir: './src/client/deploy/app',
    assetsDir: './src/client/public',
    devServerPort: 8080,
    // When using webpack-dev-server, you may need to redirect some calls
    // to a external API server. See https://webpack.js.org/configuration/dev-server/#devserver-proxy
    // Dev server proxy (array form required by current schema)
    devServerProxy: [
        {
            context: ['/api'],
            target: 'http://localhost:' + (process.env.SERVER_PROXY_PORT || '8085'),
            changeOrigin: true
        },
        {
            context: ['/socket'],
            target: 'http://localhost:' + (process.env.SERVER_PROXY_PORT || '8085'),
            changeOrigin: true,
            ws: true
        }
    ],
    // Use babel-preset-env to generate JS compatible with most-used browsers.
    // More info at https://babeljs.io/docs/en/next/babel-preset-env.html
    babel: {
        presets: [
            ['@babel/preset-env', {
                modules: false,
                // This adds polyfills when needed. Requires core-js dependency.
                // See https://babeljs.io/docs/en/babel-preset-env#usebuiltins
                // Note that you still need to add custom polyfills if necessary (e.g. whatwg-fetch)
                useBuiltIns: 'usage',
                corejs: 3
            }]
        ],
    }
}

// Determine production mode: explicit NODE_ENV=production OR --mode production and not running dev server
var isDevServer = process.argv.some(v => v.includes('webpack-dev-server'));
var isProduction = !isDevServer && (
    process.env.NODE_ENV === 'production' || process.argv.includes('production')
);
console.log('Bundling for ' + (isProduction ? 'production' : (isDevServer ? 'development (dev-server)' : 'development')) + '...');

// The HtmlWebpackPlugin allows us to use a template for the index.html page
// and automatically injects <script> or <link> tags for generated bundles.
var commonPlugins = [
    new HtmlWebpackPlugin({
        filename: 'index.html',
        template: resolve(CONFIG.indexHtmlTemplate)
    })
];

module.exports = {
    // In development, split the JavaScript and CSS files in order to
    // have a faster HMR support. In production bundle styles together
    // with the code because the MiniCssExtractPlugin will extract the
    // CSS in a separate files.
    entry: isProduction ? {
        app: [resolve(CONFIG.fsharpEntry), resolve(CONFIG.cssEntry)]
    } : {
        app: [resolve(CONFIG.fsharpEntry)],
        style: [resolve(CONFIG.cssEntry)]
    },
    // Add a hash to the output file name in production
    // to prevent browser caching if code changes
    output: {
        path: resolve(CONFIG.outputDir),
        filename: isProduction ? '[name].[fullhash].js' : '[name].js'
    },
    mode: isProduction ? 'production' : 'development',
    devtool: isProduction ? 'source-map' : 'eval-cheap-module-source-map',
    optimization: {
        runtimeChunk: 'single',
        moduleIds: 'deterministic',
        splitChunks: {
            chunks: 'all',
            maxInitialRequests: 25,
            minSize: 20000,
            cacheGroups: {
                react: {
                    test: /[\\/]node_modules[\\/](react|react-dom)[\\/]/,
                    name: 'react-vendor',
                    priority: 30,
                    enforce: true
                },
                fable: {
                    test: /[\\/]fable_modules[\\/]/,
                    name: 'fable-lib',
                    priority: 20,
                    minChunks: 2
                },
                vendors: {
                    test: /[\\/]node_modules[\\/]/,
                    name: 'vendors',
                    priority: 10,
                    reuseExistingChunk: true
                }
            }
        }
    },
    // Besides the HtmlPlugin, we use the following plugins:
    // PRODUCTION
    //      - MiniCssExtractPlugin: Extracts CSS from bundle to a different file
    //          To minify CSS, see https://github.com/webpack-contrib/mini-css-extract-plugin#minimizing-for-production
    //      - CopyWebpackPlugin: Copies static assets to output directory
    // DEVELOPMENT
    //      - HotModuleReplacementPlugin: Enables hot reloading when code changes without refreshing
    plugins: (isProduction ?
        commonPlugins.concat([
            new MiniCssExtractPlugin({
                filename: '[name].[contenthash].css',
                chunkFilename: '[id].[contenthash].css'
            }),
            // Copy static assets (favicon, audio, etc.) into production output
            new CopyWebpackPlugin({
                patterns: [
                    { from: resolve(CONFIG.assetsDir), to: '.', globOptions: { ignore: ['**/index.html'] } }
                ]
            })
        ])
        : commonPlugins.concat([
            new webpack.HotModuleReplacementPlugin(),
        ])
    ).concat(process.env.ANALYZE ? [new BundleAnalyzerPlugin({ analyzerMode: 'static', openAnalyzer: false, reportFilename: 'bundle-report.html' })] : []),
    resolve: {
        // See https://github.com/fable-compiler/Fable/issues/1490
        symlinks: false,
        extensionAlias: { '.js': ['.js'] }
    },
    // Configuration for webpack-dev-server
    devServer: {
        static: {
            directory: resolve(CONFIG.assetsDir),
            publicPath: '/'
        },
        devMiddleware: {
            publicPath: '/app',
        },
        host: '0.0.0.0',
        port: CONFIG.devServerPort,
        proxy: CONFIG.devServerProxy,
        hot: true,
    },
    // - babel-loader: transforms JS to old syntax (compatible with old browsers)
    // - sass-loaders: transforms SASS/SCSS into JS
    module: {
        rules: [
            {
                test: /\.(sass|scss|css)$/,
                use: [
                    isProduction ? MiniCssExtractPlugin.loader : 'style-loader',
                    'css-loader',
                    {
                        loader: 'resolve-url-loader',
                    },
                    {
                        loader: 'sass-loader',
                        options: {
                            implementation: require('sass'),
                            sassOptions: {
                                // Mute deprecation warnings from dependencies
                                quietDeps: true
                            }
                        }
                    }
                ],
            },
            {
                test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*)?$/,
                type: 'asset/resource'
            }
        ]
    }
};

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}