(function(global) {
  global.player = {};
})(window);

player.timoutId = null;

player.render = function render(gameState) {
  if (gameState.phase == "WaitingForPlayers") {
    player.renderWaiting(gameState.players);
  }
}

player.renderWaiting = function renderWaiting(players) {
  console.log(players);
}

console.log("hi");
