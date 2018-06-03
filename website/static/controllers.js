(function() {
  var app = angular.module('lizardApp', []);

  app.controller('PhoneListCtrl', function ($scope) {
  });

  app.controller('SourceFormController', ['$scope', '$http', function ($scope, $http) {
    $scope.source = {};
    $scope.submit = function () {
      $scope.progress = "analysing...";
      $scope.result = {};
      $http.post("/analyze", $scope.source).success(function(data) {
        $scope.progress = "Code analyzed successfully.";
        $scope.result = data;
      }).error(function() {
        $scope.progress = "Error connecting the analysing server.";
      });
   };
    $scope.progress = "Example Result";
   $scope.result = {"token_count":307,"filename":".c","function_list":[{"parameter_count":1,"end_line":8,"nloc":3,"cyclomatic_complexity":1,"start_line":6,"token_count":22,"long_name":"isAlive( const Cell & cell ) const","name":"isAlive"},{"parameter_count":1,"end_line":13,"nloc":4,"cyclomatic_complexity":1,"start_line":10,"token_count":22,"long_name":"withAlive( const Cell & cell )","name":"withAlive"},{"parameter_count":1,"end_line":21,"nloc":7,"cyclomatic_complexity":3,"start_line":15,"token_count":59,"long_name":"getNeighborCountOf( const Cell & cell ) const","name":"getNeighborCountOf"},{"parameter_count":1,"end_line":26,"nloc":4,"cyclomatic_complexity":2,"start_line":23,"token_count":31,"long_name":"has2Or3AliveNeighbors( const Cell & cell ) const","name":"has2Or3AliveNeighbors"},{"parameter_count":2,"end_line":31,"nloc":4,"cyclomatic_complexity":2,"start_line":28,"token_count":30,"long_name":"tickOfAliveCell( const Cell & cell , GameOfLife & gameOfNextTick ) const","name":"tickOfAliveCell"},{"parameter_count":2,"end_line":37,"nloc":5,"cyclomatic_complexity":3,"start_line":33,"token_count":68,"long_name":"tickOfNeighbors( const Cell & cell , GameOfLife & gameOfNextTick ) const","name":"tickOfNeighbors"},{"parameter_count":0,"end_line":47,"nloc":8,"cyclomatic_complexity":2,"start_line":39,"token_count":59,"long_name":"tick( ) const","name":"tick"}],"nloc":46};
  }]);

})();
