import webapp2
import json
from json import JSONEncoder
class FileInfoEncoder(JSONEncoder):
        def default(self, o):
                    return o.__dict__
from lizard import analyze_file

class LizardAnalyzer(webapp2.RequestHandler):
    def post(self):
        dictionary = json.loads(self.request.body)
        file_info = analyze_file.analyze_source_code(dictionary.get("lang", ".c"), dictionary.get("code", ""))
        self.response.headers['Content-Type'] = 'application/json'
        self.response.write(json.dumps(file_info, cls=FileInfoEncoder))

application = webapp2.WSGIApplication([
    ('/analyze', LizardAnalyzer),
], debug=True)

