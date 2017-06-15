# MCS_Sentibot
A slack bot reacting to messages expressing sentiments. With a list of known entries, the bot can "tell" in what state of mind a given user is and writes a message describing his current sentiment. 

The entries are a regex and its corresponding emotion. For example, if the bot has the regex `"I('m|am) happy"` that is mapped to the emotion `happy`, when any user writes `I'm happy` or `I am happy`, the bot will send a message containing `@USERNAME is happy`, with `USERNAME` being the username of the user.

The entries of the bot are fully configurable with commands. The complete list can be found in the `Usage` section.

### Requirements
- Erlang 19.X
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

## Usage
To interact with the bot, you can use the following comands:
```
@BOT_USERNAME COMMAND [PARAMETERS]...

Available Commands:
    help                           Display this help message
    add REGEX SENTIMENT            Add a new sentiment recognition at the begining of the list
    delete INDEX                   Delete the rule at the given index
    dump                           Dump the current rules
    insert INDEX REGEX SENTIMENT   Add a new sentiment recognition at the given INDEX
    move OLD_INDEX NEW_INDEX       Move the rule from OLD_INDEX to NEW_INDEX
    save                           Save the current rules
    state                          Show the state of mind of peoples in the channel
```
Where `BOT_USERNAME` is the username of the bot and `COMMAND` one of the listed commands.
