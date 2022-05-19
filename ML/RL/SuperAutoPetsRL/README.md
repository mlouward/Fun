# Super Auto Pets RL

Project to create a clone of [Super Auto Pets](https://teamwoodgames.com/), intended to use for RL.

**Roadmap**:

- [x] Create the game mechanics, including the shop, the battle system, the player actions
  - [x] Get the data for all animals
  - [ ] Get the data for all foods
  - [ ] Shop system
    - [ ] Reroll shop/Geenrate new shop at start of round
      - [x] Generate random animals for the current turn (shop tier)
      - [ ] Generate random foods for the current turn (shop tier)
    - [ ] Buy/sell/freeze a pet
      - [x] Freeze a pet
      - [ ] Buy a pet
      - [ ] Sell a pet
    - [ ] Buy/sell/freeze a food
      - [x] Freeze a food
      - [ ] Buy a food
      - [ ] Sell a food
    - [ ] Combine/level up animals
    - [ ] Apply the Start of Turn/Buy... triggers
  - [ ] Battle system
    - [ ] Apply the Start of Battle/Summon... triggers
    - [ ] Run the attack sequence of the pets (+triggers)
    - [ ] Apply the health changes/round win changes at the end of the round

- [ ] Implement RL algorithms using TensorFlow, study different algorithms and compare the results
  - [ ] Monte Carlo
  - [ ] Policy iteration
  - [ ] (Deep) Q-learning
  - [ ] Sarsa

- [ ] Have a way to input your current game stats (health, round, pets, stats, etc.) and give a prediction of the best action to take using one of the algorithms above.
