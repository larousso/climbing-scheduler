module.exports = {
  entry: {
    home: './src/home/home.js',
    user: './src/user/User.js',
  },
  output: {
    path: __dirname + '/dist',
    publicPath: '/static/',
    pathinfo: true,
    filename: '[name].js',
  },
  resolve: {
      modules: ['node_modules'],
      extensions: ['.js', '.elm']
  },
  devServer: {
    port: process.env.DEV_SERVER_PORT || 3000
  },
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: {}
      }
    }, {
      test: /\.js$/,
      exclude: /(node_modules|bower_components)/,
      use: {
        loader: 'babel-loader',
        options: {
          presets: ['env']
        }
      }
    }]
  }
};
