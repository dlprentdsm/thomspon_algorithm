# thomspon_algorithm
Fun with the thomspon algorithm

The file ```thompson sampling.R``` is just a fun worksheet to run that builds up the thompson algorithm from basic bayesian ideas and does some simulations. 

The folder ```thompson_plumber``` demonstrates an implementation of a thomspon sampling API for production use. When a client needs to take an action, it can get an action from the API, and then whenever it learns the result of that action it can update the data for that action.

To play with the API, use:
```cd thompson_plumber && docker-compose up --build -d && docker-compose logs -f```
The API will run at ```0.0.0.0:41333```, and the swagger is at ```http://0.0.0.0:41333/__swagger__/```

Then you can use the script ```test_thompson.R``` to simulate a campaign run using the API.
