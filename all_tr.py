from flask import Flask
from flask import request
import time
import json
from steem import Steem


app = Flask(__name__)
 
@app.route("/get_all_tr", methods=['POST'])
def get_all_tr():
    global s
    data = request.get_json()
    print(data)
    name = data['name']
    
    osszes=[]
    szamlalo=10000

    while True:
        try:
            print(szamlalo)
            t = s.steemd.get_account_history(name, index_from=szamlalo, limit=10000)
            
        except Exception as e:
            print(e)
            time.sleep(5)
            s = Steem()
            t = s.steemd.get_account_history(name, index_from=szamlalo, limit=10000)
            
            
        if osszes:
            utolso = osszes[-1][0]
        else:
            utolso=t[-1][0]

        if(t[-1][0]!=szamlalo):
            kell_meg = t[-1][0]-utolso
            osszes.extend(t[-kell_meg:])
            break

        osszes.extend(t[0:-1])
        szamlalo+=10000

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


