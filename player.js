var DeckAround = React.createClass({
  loadGameStateFromServer: function() {
    $.ajax({
      url: this.props.url,
      dataType: 'json',
      success: function(game) {
        this.setState({game: game});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error(this.props.url, status, err.toString());
      }.bind(this)
    });
  },
  onJoin: function(player) {
    $.ajax({
      url: 'join',
      type: 'POST',
      dataType: 'json',
      data: {name: player},
      success: function(game) {
        this.setState({
          game: game,
          player: player
        });
      }.bind(this),
      error: function(xhr, status, err) {
        console.error('join', status, err.toString());
      }.bind(this)
    });
  },
  onDeal: function() {
    $.ajax({
      url: 'deal',
      type: 'POST',
      dataType: 'json',
      success: function(game) {
        this.setState({game: game});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error('deal', status, err.toString());
      }.bind(this)
    });
  },
  onPrompt: function(prompt) {
    $.ajax({
      url: 'prompt',
      type: 'POST',
      dataType: 'json',
      data: {prompt: prompt},
      success: function(game) {
        this.setState({game: game});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error('prompt', status, err.toString());
      }.bind(this)
    });
  },
  onDefine: function(definition) {
    console.log(this.state.player, "submitted", definition);
    $.ajax({
      url: 'define',
      type: 'POST',
      dataType: 'json',
      data: {definition: definition, name: this.state.player},
      success: function(game) {
        this.setState({game: game});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error('prompt', status, err.toString());
      }.bind(this)
    });
  },
  onStartVoting: function() {
    $.ajax({
      url: 'move-to-vote',
      type: 'POST',
      dataType: 'json',
      success: function(game) {
        this.setState({game: game});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error('move-to-vote', status, err.toString());
      }.bind(this)
    });
  },
  onVote: function(voter, votee) {
    $.ajax({
      url: 'vote',
      type: 'POST',
      dataType: 'json',
      data: {voter: voter, votee: votee},
      success: function(game) {
        this.setState({game: game});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error('vote', status, err.toString());
      }.bind(this)
    });
  },
  onEndVoting: function() {
    $.ajax({
      url: 'tally-votes',
      type: 'POST',
      dataType: 'json',
      success: function(game) {
        this.setState({game: game});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error('tally-votes', status, err.toString());
      }.bind(this)
    });
  },
  onReset: function() {
    $.ajax({
      url: 'reset',
      type: 'POST',
      dataType: 'json',
      success: function(game) {
        this.setState({game: game});
      }.bind(this),
      error: function(xhr, status, err) {
        console.error('reset', status, err.toString());
      }.bind(this)
    });
  },
  getInitialState: function() {
    return {game: {phase: "NotConnected"}};
  },
  componentDidMount: function() {
    this.loadGameStateFromServer();
    setInterval(this.loadGameStateFromServer, this.props.pollInterval);
  },
  render: function() {
    return {
      "NotConnected": <NotConnected />,
      "WaitingForPlayers": <WaitingForPlayers
        players={this.state.game.players} player={this.state.player}
        onJoin={this.onJoin} onDeal={this.onDeal} />,
      "Dealing": <Dealing dealer={this.state.game.dealer}
        player={this.state.player} onPrompt={this.onPrompt} />,
      "Defining": <Defining
        dealer={this.state.game.roundInProgress &&
          this.state.game.roundInProgress.dealer}
        player={this.state.player}
        definitions={this.state.game.roundInProgress &&
          this.state.game.roundInProgress.definitions}
        onDefine={this.onDefine} onStartVoting={this.onStartVoting} />,
      "Voting": <Voting player={this.state.player}
        dealer={this.state.game.roundInProgress &&
          this.state.game.roundInProgress.dealer}
        definitions={this.state.game.roundInProgress &&
          this.state.game.roundInProgress.definitions} onVote={this.onVote}
          onEndVoting={this.onEndVoting} />,
      "EndOfRound": <EndOfRound player={this.state.player}
        players={this.state.game.game && this.state.game.game.players}
        rounds={this.state.game.game && this.state.game.game.rounds}
        onDeal={this.onDeal} />,
      "Over": <Over player={this.state.player}
        players={this.state.game.game && this.state.game.game.players}
        rounds={this.state.game.game && this.state.game.game.rounds}
        onReset={this.onReset} />
    }[this.state.game.phase] || <div>unhandled game phase: {this.state.game.phase}</div>;
  }
});

var NotConnected = React.createClass({
  render: function() {
    return (
      <div className="NotConnected">
        Not connected to the server, trying to connect...
      </div>
    );
  }
});

var WaitingForPlayers = React.createClass({
  deal: function(e) {
    e.preventDefault();
    this.props.onDeal();
  },
  render: function() {
    var playerList = this.props.players.map(function(player) {
      return (
        <div className="player">
          {player}
        </div>
      );
    });
    var join = <JoinForm onJoin={this.props.onJoin} />;
    var deal = (
      <form onSubmit={this.deal}>
        <input type="submit" value="Deal" />
      </form>
    );
    return (
      <div className="waitingForPlayers">
        <h1>Deck Around</h1>
        {-1 < this.props.players.indexOf(this.props.player) ? '' : join}
        {this.props.players.length} Players playing.
        {playerList}
        {3 <= this.props.players.length && this.props.players[0] == this.props.player ? deal : ''}
      </div>
    );
  }
});

var JoinForm = React.createClass({
  joinGame: function(e) {
    e.preventDefault();
    var name = this.refs.name.getDOMNode().value.trim();
    this.props.onJoin(name);
  },
  render: function() {
    return (
      <form className="joinForm" onSubmit={this.joinGame}>
        <input type="text" placeholder="Your name" ref="name" />
        <input type="submit" value="Join" />
      </form>
    );
  }
});

var Dealing = React.createClass({
  render: function() {
    return this.props.dealer == this.props.player ? (
      <div>
        <h1>Draw a card</h1>
        <PromptForm onPrompt={this.props.onPrompt} />
      </div>
      ) : (
      <h1>Hold tight - {this.props.dealer} is drawing a card.</h1>
    );
  }
});

var PromptForm = React.createClass({
  statePrompt: function(e) {
    e.preventDefault();
    var word = this.refs.word.getDOMNode().value.trim();
    this.props.onPrompt(word);
  },
  render: function() {
    return (
      <form className="promptForm" onSubmit={this.statePrompt}>
        <input type="text" placeholder="The word to define" ref="word" />
        <input type="submit" value="Share" />
      </form>
    );
  }
});

var Defining = React.createClass({
  render: function() {
    var playerIsTheDealer = this.props.dealer == this.props.player;
    var theDealerDefinitionExists = -1 < this.props.definitions.map(
        function(def) {
      return def.author;
    }).indexOf(this.props.dealer);
    var thereAreAtLeastThreeDefinitions = 3 <= this.props.definitions.length;
    return (
      <div>
        <DefiningForm onDefine={this.props.onDefine} />
        {playerIsTheDealer && theDealerDefinitionExists &&
          thereAreAtLeastThreeDefinitions ? <StartVoting
          onStartVoting={this.props.onStartVoting} /> : ''}
      </div>
    );
  }
});

var DefiningForm = React.createClass({
  onDefine: function(e) {
    e.preventDefault();
    var definition = this.refs.definition.getDOMNode().value.trim();
    this.props.onDefine(definition);
  },
  render: function() {
    return (
      <form className="defineForm" onSubmit={this.onDefine}>
        <textarea placeholder="Write a definition" ref="definition"></textarea>
        <input type="submit" value="Submit" />
      </form>
    );
  }
});

var StartVoting = React.createClass({
  onStartVoting: function(e) {
    e.preventDefault();
    this.props.onStartVoting();
  },
  render: function() {
    return (
      <form className="startVotingForm" onSubmit={this.onStartVoting}>
        <input type="submit" value="Begin voting" />
      </form>
    );
  }
});

var Voting = React.createClass({
  onEndVoting: function(e) {
    e.preventDefault();
    this.props.onEndVoting();
  },
  render: function() {
    var amDealer = this.props.player == this.props.dealer;
    var definitions = this.props.definitions.filter(function(definition) {
      return amDealer || definition.author != this.props.player;
    }.bind(this));
    if (amDealer) {
      definitionElements = definitions.map(function(definition) {
        return <p>{definition.definition}</p>;
      });
      return (
        <div>
          {definitionElements}
          <button onClick={this.onEndVoting}>End Voting</button>
        </div>
      );
    };
    return <VoteForm player={this.props.player} definitions={definitions}
      onVote={this.props.onVote} />;
  }
});

var VoteForm = React.createClass({
  onVote: function(votee) {
    return function(e) {
      e.preventDefault();
      this.props.onVote(this.props.player, votee);
    }.bind(this);
  },
  render: function() {
    var definitions = this.props.definitions.map(function(definition) {
      return <input type="button" onClick={this.onVote(definition.author)}
        value={definition.definition} />;
    }.bind(this));
    return (
      <div>
        {definitions}
      </div>
    );
  }
});

var EndOfRound = React.createClass({
  onDeal: function(e) {
    e.preventDefault();
    this.props.onDeal();
  },
  render: function() {
    var lastRound = this.props.rounds[this.props.rounds.length - 1];
    var scores = endOfRoundSummary(this.props.players, this.props.rounds);
    return (
      <div>
        {renderRoundSummary(scores)}
        {this.props.player == lastRound.dealer ?
          <input type="button" onClick={this.onDeal}
            value="Start next round" /> : ''}
      </div>
    );
  }
});

var Over = React.createClass({
  onReset: function(e) {
    e.preventDefault();
    this.props.onReset();
  },
  render: function() {
    var lastRound = this.props.rounds[this.props.rounds.length - 1];
    var scores = endOfRoundSummary(this.props.players, this.props.rounds);
    return (
      <div>
        {renderRoundSummary(scores)}
        <input type="button" onClick={this.onReset} value="Start new game" />
      </div>
    );
  }
});

function playerScoreInRound(player, round) {
  if (round.dealer == player) {
    return 0;
  }
  var timesVotedFor = round.votes.filter(function(vote) {
    return vote.votee == player;
  }).length;
  var voteForRound = round.votes.filter(function(vote) {
    return vote.voter == player;
  })[0];
  var votedCorrectly = !!(voteForRound && voteForRound.votee == round.dealer);
  return 2 * timesVotedFor + +votedCorrectly;
}

function endOfRoundSummary(players, rounds) {
  var lastRound = rounds[rounds.length - 1];
  var scores = {};
  players.forEach(function(player) {
    scores[player] = {};
  });
  lastRound.definitions.forEach(function(definition) {
    scores[definition.author].definition = definition.definition;
  });
  players.forEach(function(player) {
    scores[player].roundScore = playerScoreInRound(player, lastRound);
    scores[player].totalScore = rounds.map(function(round) {
      return playerScoreInRound(player, round);
    }).reduce(function(acc, cur) {
      return acc + cur;
    });
  });
  return scores;
}

function renderRoundSummary(summary) {
  var playerRows = Object.keys(summary).map(function(player) {
    score = summary[player];
    return (
      <tr>
        <td>{player}</td>
        <td>{score.roundScore}</td>
        <td>{score.totalScore}</td>
        <td>{score.definition}</td>
      </tr>
    );
  });
  return (
    <table>
      <tr>
        <th>Player</th>
        <th>Round</th>
        <th>Total</th>
        <th>Definition</th>
      </tr>
      {playerRows}
    </table>
  );
}

React.render(
  <DeckAround url="status" pollInterval={1000}/>,
  document.getElementById('content')
);
