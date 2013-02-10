Test the app with curl

curl -v -X POST http://127.0.0.1:8080/compute -H 'Content-Type: application/json' --data @src/test/resources/json/compute.json
