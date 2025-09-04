class Request:
    def __init__(self):
        self.body = ''

class Response:
    def __init__(self):
        self.headers = {}
        self._body = []
    def write(self, s):
        self._body.append(s)

class RequestHandler:
    def __init__(self):
        self.request = Request()
        self.response = Response()

class WSGIApplication:
    def __init__(self, routes, debug=False):
        self.routes = routes
        self.debug = debug