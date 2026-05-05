import { Amplify } from 'aws-amplify';
import { generateClient } from "aws-amplify/api"

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

function configure(endpoint, region, apikey) {
    var cfg = {
        API: {
            GraphQL: {
                endpoint: endpoint,
                region: region,
                defaultAuthMode: 'apiKey',
                apiKey: apikey
            }
        }
    };

    Amplify.configure(cfg);
}

var client = generateClient()

var nextSubscriptionId = 1
var subsciptions = {}

function subscribe(quizId, token, onSuccess, onError) {
    var subscription = client.graphql(
        {
            query: onQuizMessage,
            variables: { quizId: quizId, token: token }
        }
    ).subscribe({
        next: (todoData) => {
            onSuccess(todoData.data.onQuizMessage)
        },
        error: error => {
            console.warn(error);
            onError(error.errors)
        },
    });

    subsciptions[nextSubscriptionId] = subscription
    return nextSubscriptionId++
}

function unsubscribe(subscriptionId) {
    subsciptions[subscriptionId].unsubscribe();
    delete subsciptions[subscriptionId]
}

export { configure, subscribe, unsubscribe };