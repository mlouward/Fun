# Super Auto Pets RL

Project to create a clone of [Super Auto Pets](https://teamwoodgames.com/), intended to use for RL.

**Roadmap**:

- [x] Create the game mechanics, including the shop, the battle system, the player actions
  - [x] Get the data for all animals
  - [x] Get the data for all foods, pets, statuses
  - [ ] Shop system
    - [x] Reroll shop/Generate new shop at start of round
      - [x] Generate random animals for the current turn (shop tier)
      - [x] Generate random foods for the current turn (shop tier)
    - [x] Buy/sell/freeze a pet
      - [x] Freeze a pet
      - [x] Buy a pet (Triggers not done yet)
      - [x] Sell a pet (Triggers not done yet)
    - [ ] Buy/sell/freeze a food
      - [x] Freeze a food
      - [ ] Buy a food
    - [x] Combine/level up animals
    - [ ] Apply the Start of Turn/Buy... triggers
  - [ ] Battle system
    - [ ] Apply the Start of Battle/Summon... triggers
    - [ ] Run the attack sequence of the pets (+triggers)
    - [ ] Apply the health changes/round win changes at the end of the round
  - [ ] Add testing ?

- [ ] Implement RL algorithms using TensorFlow, study different algorithms and compare the results
  - [ ] Monte Carlo
  - [ ] Policy iteration
  - [ ] (Deep) Q-learning
  - [ ] Sarsa

- [ ] Have a way to input your current game stats (health, round, pets, stats, etc.) and give a prediction of the best action to take using one of the algorithms above.
