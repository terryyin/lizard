(function() {
  var app = angular.module('lizardApp', []);

  app.controller('PhoneListCtrl', function ($scope) {
  });

  app.controller('SourceFormController', ['$scope', '$http', function ($scope, $http) {
    $scope.source = {};
    $scope.source.lang = '.java';
    $scope.submit = function () {
      $scope.progress = "analysing...";
      $scope.result = {
        function_list: [
          {
            name: "abc",
            nloc: 10
          }
        ]
      };

      $http.post("/analyze", $scope.source).success(function(data) {
        $scope.progress = "Code analyzed successfully.";
        $scope.result = data;
      }).error(function() {
        $scope.progress = "Error connecting the analysing server.";
      });
   };
  }]);

})();
