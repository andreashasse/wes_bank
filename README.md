http POST localhost:8080 User-Id:hej -j
http POST localhost:8080/a1/insert User-Id:hej amount:=10 -j
http GET localhost:8080/a1/balance User-Id:hej -j
http POST localhost:8080/a1/transfer User-Id:hej to=a2 amount:=10 -j