const webpack = require('webpack');

module.exports = {
  webpack: {
    configure: (webpackConfig) => {
      webpackConfig.resolve = {
        ...webpackConfig.resolve,
        fallback: {
          ...webpackConfig.resolve.fallback,
          buffer: require.resolve('buffer/'),
          crypto: require.resolve('crypto-browserify'),
          stream: require.resolve('stream-browserify'),
          path: require.resolve('path-browserify'),
		  vm: require.resolve('vm-browserify'),
		  process: require.resolve('process/browser.js'),
		  fs: false,
		  dgram: false,
		  zlib: require.resolve('browserify-zlib'),
		  url: require.resolve("url/"),
		  https: require.resolve("https-browserify"),
		  os: require.resolve("os-browserify"),
		  http: require.resolve("stream-http") 
        },
      };
      webpackConfig.plugins = [
        ...(webpackConfig.plugins || []),
        new webpack.ProvidePlugin({
          Buffer: ['buffer', 'Buffer'],	
          process: 'process/browser.js', 		  
        }),
      ];
      return webpackConfig;
    },
  },
};
