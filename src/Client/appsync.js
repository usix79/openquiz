import Amplify, {API, graphqlOperation } from 'aws-amplify';


const onQuizMessage = `
  subscription OnQuizMessage($quizId: Int!, $token: String!) {
    onQuizMessage(quizId: $quizId, token: $token) {
      #quizId
      #token
      body
      #version
    }
  }
`;

function configure (endpoint, region, apikey){
    var awsmobile = {
        "aws_appsync_graphqlEndpoint": endpoint,
        "aws_appsync_region": region,
        "aws_appsync_authenticationType": "API_KEY",
        "aws_appsync_apiKey": apikey
    };

    Amplify.configure(awsmobile);
}

var nextSubscriptionId = 1
var subsciptions = {}

function subscribe (quizId, token, onSuccess, onError){
    var subscription = API.graphql(
        graphqlOperation(onQuizMessage, {quizId: quizId, token: token})
    ).subscribe({
        next: (todoData) => {
          onSuccess(todoData.value.data.onQuizMessage)
        },
        error: error => {
            console.warn(error);
            onError (error.error.errors)
        },
    });

    subsciptions[nextSubscriptionId] = subscription
    return nextSubscriptionId++
}

function unsubscribe (subscriptionId){
    subsciptions[subscriptionId].unsubscribe();
    delete subsciptions[subscriptionId]
}

export { configure, subscribe, unsubscribe };