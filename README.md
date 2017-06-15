# MCS_Sentibot
A slack bot reacting to messages expressing emotions.


### Requirements
- Erlang
- Rebar3
- A Slack token for the server you want to run the bot on.

#### Getting your Slack token
1. Go to the Slack website and select your server.
2. Go to the "manage" page on the top menu. From there, you can go to "cutom integration" on the menu on the left, and then select "bots".
3. Add a configuration, set the username of the bot and you can get your token.

## Deployment
1. Clone or download the repo and `cd` into it.
2. before launching the bot, you will need to configure it. In the `config` folder, there is a sample config file, you can duplicate it and rename it "sys.config". You will need to put your Slack token there.
3. `$ rebar3 auto`: it will download the dependencies and start the bot.
4. The bot joins the server. By default, the bot isn't in any channel. To invite it from the app, got to `channel settings > invite team members to join` and enter the bot username. It will join the channel.
5. You're done! You can now interact with the bot.
