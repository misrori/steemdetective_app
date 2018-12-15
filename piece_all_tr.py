from flask import Flask
from flask import request
import time
import json
from steem import Steem
import re
# steem install https://steemit.com/computer/@fr3eze/steem-pythoninstallationonlinux-1ryocg7ebu?sort=author_reputation

app = Flask(__name__)
 
@app.route("/get_all_tr", methods=['POST'])
def get_all_tr():
    global s
    data = request.get_json()
    print(data)
    name = data['name']
    my_to=data['to']
    my_limit=data['limit']
    
    osszes = s.steemd.get_account_history(name, index_from=my_to, limit=my_limit)
    

    return app.response_class(
        response=json.dumps(osszes),
        status=200,
        mimetype='application/json' )

@app.route("/get_all_tr_number", methods=['POST'])
def get_all_tr_number():
    data = request.get_json()
    print(data)
    name = data['name']

    t = s.steemd.get_account_history(name,index_from=-1, limit=1 )

    szam = {'all_tr_number':t[-1][0]+1 }
    return app.response_class(
        response=json.dumps(szam),
        status=200,
        mimetype='application/json' )

 
if __name__ == "__main__":
    s = Steem()
    app.run(port= 54321, threaded=True)


