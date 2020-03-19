# env FLASK_APP=../pcap_upload_endpoint flask run

# how to use: curl -F "file=@pcaps/game_tcp_10.60.181.2_20191123_185813.pcap" http://localhost:5000/
import os
from flask import Flask, flash, request, redirect, url_for
from werkzeug.utils import secure_filename

UPLOAD_FOLDER = os.getenv("UPLOAD_FOLDER") or './'
PASSWORD = os.getenv("PASSWORD") or ""

print("UPLOAD_FOLDER = ", UPLOAD_FOLDER)
print("ENDPOINT = ", PASSWORD)

app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER


@app.route('/' + PASSWORD, methods=['GET', 'POST'])
def upload_file():
    if request.method == 'POST':
        if 'file' not in request.files:
            print('No file part')
            return redirect(request.url)
        file = request.files['file']
        if file.filename == '':
            print('No selected file')
            return redirect(request.url)
        if file:
            filename = secure_filename(file.filename)
            file.save(os.path.join(app.config['UPLOAD_FOLDER'], filename))
            print('Done:', filename)
            return redirect(request.url)
    return '''
    <!doctype html>
    <title>Upload new File</title>
    <h1>Upload new File</h1>
    <form method=post enctype=multipart/form-data>
      <input type=file name=file>
      <input type=submit value=Upload>
    </form>
    '''
