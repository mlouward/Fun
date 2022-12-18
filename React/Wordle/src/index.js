import React from "react";
import ReactDOM from "react-dom";
import Select from "react-select";
import "./index.css";
import words5_en from "./assets/wordlists/words5_en.txt";
import words5_fr from "./assets/wordlists/words5_fr.txt";
import words6_fr from "./assets/wordlists/words6_fr.txt";
import words7_fr from "./assets/wordlists/words7_fr.txt";
import words8_fr from "./assets/wordlists/words8_fr.txt";

// Game of Wordle: given a secret word, the user must guess the word
const MAX_GUESSES = 7;
let WORD_LENGTH = 0;
let GAME_WORDLIST = null;

const options = [
    { value: { path: words5_en, count: 1500 }, label: "English 5 letters" },
    { value: { path: words5_fr, count: 2000 }, label: "French 5 letters" },
    { value: { path: words6_fr, count: 6000 }, label: "French 6 letters" },
    { value: { path: words7_fr, count: 6000 }, label: "French 7 letters" },
    { value: { path: words8_fr, count: 8000 }, label: "French 8 letters" },
];

// Square class
function Square(props) {
    return (
        <div
            className="square"
            onKeyDown={props.onKeyDown}
            style={{
                backgroundColor: props.backgroundColor,
                border: props.border,
            }}
        >
            {props.value}
        </div>
    );
}

// Grid class of MAX_GUESSES lines and WORD_LENGTH columns
class Grid extends React.Component {
    renderSquare(i, j, color) {
        const letter = this.props.guessedLetters[i][j];
        return (
            <Square
                onKeyDown={this.props.onKeyDown}
                key={i * this.props.size + j}
                value={letter ? letter : " "}
                backgroundColor={color}
                border={
                    this.props.guessedLetters[i][j] ? "0.2rem solid #222" : ""
                }
            />
        );
    }

    render() {
        // Grid of "size" columns and MAX_GUESSES rows
        if (GAME_WORDLIST === null) {
            return null;
        }
        return (
            <div className="grid">
                {Array(MAX_GUESSES)
                    .fill(null)
                    .map((_, i) => (
                        <div className="grid-row" key={i}>
                            {Array(WORD_LENGTH)
                                .fill(null)
                                .map((_, j) =>
                                    this.renderSquare(
                                        i,
                                        j,
                                        this.props.colors[i][j]
                                    )
                                )}
                        </div>
                    ))}
            </div>
        );
    }
}

class Game extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            secretWord: null,
            guessedLetters: [[], [], [], [], [], [], []],
            colors: Array(MAX_GUESSES)
                .fill(null)
                .map(() => Array(WORD_LENGTH).fill("transparent")),
            numberOfGuesses: 0,
            gameStatus: "playing",
        };
    }

    async componentDidMount() {
        // get words from file
        this.getWordsFromFile(options[0]);
        // Event listener for user input
        document.addEventListener("keydown", (event) =>
            this.handleKeyDown(event)
        );
    }

    getWordsFromFile = async (selection) => {
        // fetch words from text file and save them in uppercase
        GAME_WORDLIST = selection.value.path;
        console.log(GAME_WORDLIST);
        this.allWords = await fetch(GAME_WORDLIST)
            .then((data) => data.text())
            .then((text) => {
                return text.split("\r\n").map((x) => x.toUpperCase());
            });
        // set WORD_LENGTH to the length of the words in the list
        WORD_LENGTH = this.allWords[0].length;
        // We keep only 2000 words, the others are too uncommon
        this.playableWords = this.allWords.slice(0, selection.value.count);
        // set the word for current game
        this.setWord();
    };

    getLettersColors() {
        const lastWordColors =
            this.state.colors[this.state.numberOfGuesses].slice();
        const secretWord = this.state.secretWord.split("");
        const guessedLetters =
            this.state.guessedLetters[this.state.numberOfGuesses].slice();

        // Letters in the right place first
        for (let index = 0; index < guessedLetters.length; index++) {
            if (guessedLetters[index] === secretWord[index]) {
                lastWordColors[index] = "rgb(76, 145, 87)";
                // replace letter already found with a placeholder
                secretWord[index] = "_";
                // replace guessed letter with a different placeholder
                guessedLetters[index] = " ";
            }
        }
        // Letters misplaced but present in the word
        for (let index = 0; index < guessedLetters.length; index++) {
            const guessedLetter = guessedLetters[index];
            if (secretWord.includes(guessedLetter)) {
                lastWordColors[index] = "rgb(181, 159, 59)";
                // replace letter already found with a placeholder
                secretWord[secretWord.indexOf(guessedLetter)] = "_";
            }
        }

        const newColors = this.state.colors.slice();
        newColors[this.state.numberOfGuesses] = lastWordColors.map((color) =>
            color === "transparent" ? "rgb(58, 58, 60)" : color
        );
        return newColors;
    }

    handleUserWord() {
        const currentWord =
            this.state.guessedLetters[this.state.numberOfGuesses].join("");
        // Color every letter of the word green if it is correctly placed and yellow if it is in the word but not in the right place
        const newColors = this.getLettersColors();
        if (currentWord === this.state.secretWord) {
            // Check if the word is correct
            this.setState({
                gameStatus: "won",
                colors: newColors,
                numberOfGuesses: this.state.numberOfGuesses + 1,
            });
        } else if (this.state.numberOfGuesses === MAX_GUESSES - 1) {
            // Check if too many guesses
            this.setState({
                gameStatus: "lost",
                colors: newColors,
                numberOfGuesses: this.state.numberOfGuesses + 1,
            });
        } else {
            // Continue the game otherwise
            // Check if word is in the list of words
            if (!this.allWords.includes(currentWord)) {
                // display an alert if the word is not in the list
                alert(`${currentWord} is not a known word`);
                // clear the guessed word
                const newGuessedLetters = this.state.guessedLetters.slice();
                newGuessedLetters[this.state.numberOfGuesses] = [];
                this.setState({
                    guessedLetters: newGuessedLetters,
                });
                return;
            }
            this.setState({
                gameStatus: "playing",
                colors: newColors,
                numberOfGuesses: this.state.numberOfGuesses + 1,
            });
        }
    }

    handleUserLetterInput(event) {
        // add letter to guessedLetters
        const newGuessedLetters = this.state.guessedLetters.slice();
        // check if word is not more than 5 letters
        if (
            !(
                newGuessedLetters[this.state.numberOfGuesses].length >=
                WORD_LENGTH
            )
        ) {
            const afterGuess = this.state.guessedLetters[
                this.state.numberOfGuesses
            ].concat(event.key.toUpperCase());
            newGuessedLetters[this.state.numberOfGuesses] = afterGuess;
            this.setState({
                guessedLetters: newGuessedLetters,
            });
        }
    }

    handleKeyDown(event) {
        // If status is not "playing", do nothing
        if (this.state.gameStatus !== "playing") {
            return;
        }
        // Test if word is 5 letters long
        if (event.key === "Backspace") {
            // Remove last letter from guessedLetters
            if (this.state.guessedLetters.length > 0) {
                // Remove last letter from guessedLetters at index numberOfGuesses
                const newGuessedLetters = this.state.guessedLetters.slice();
                newGuessedLetters[this.state.numberOfGuesses].pop();

                this.setState({
                    guessedLetters: newGuessedLetters,
                });
            }
        } else if (/^[A-Z]{1}$/i.test(event.key)) {
            // Add letter to guessedLetters
            this.handleUserLetterInput(event);
            if (
                this.state.guessedLetters[this.state.numberOfGuesses].length ===
                WORD_LENGTH
            ) {
                // If word is 5 letters long, check if it is correct
                this.handleUserWord();
            }
        }
    }

    renderResult() {
        switch (this.state.gameStatus) {
            case "won":
                return <div>You won!</div>;
            case "lost":
                return (
                    <div>You lost! The word was {this.state.secretWord}</div>
                );
            default:
                return <div>Guess the word !</div>;
        }
    }

    setWord = () => {
        console.log();
        // Select a random word
        const secretWord =
            this.playableWords[
                Math.floor(Math.random() * this.playableWords.length)
            ];
        console.log(secretWord);
        // Initialize the game
        this.setState({
            secretWord: secretWord,
            guessedLetters: [[], [], [], [], [], [], []],
            colors: Array(MAX_GUESSES)
                .fill(null)
                .map(() => Array(WORD_LENGTH).fill("transparent")),
            numberOfGuesses: 0,
            gameStatus: "playing",
        });
    };

    render() {
        return (
            <div className="game">
                <header>
                    <h1>Guess the Word!</h1>
                </header>
                <div className="selection-menu">
                    <Select
                        onChange={this.getWordsFromFile}
                        options={options}
                        isSearchable={false}
                        defaultValue={options[0]}
                        hideSelectedOptions
                    />{" "}
                </div>
                <div className="game-board">
                    <Grid
                        size={WORD_LENGTH}
                        onKeyDown={this.handleKeyDown}
                        guessedLetters={this.state.guessedLetters}
                        numberOfGuesses={this.state.numberOfGuesses}
                        secretWord={this.state.secretWord}
                        colors={this.state.colors}
                    />
                </div>
                <div className="game-info">
                    <div className="game-info-cell">{this.renderResult()}</div>
                    <div className="game-info-cell">
                        {this.state.numberOfGuesses}/{MAX_GUESSES}
                    </div>
                    <div className="game-info-cell">
                        <button onClick={this.setWord}>
                            Play another word !
                        </button>
                    </div>
                </div>
            </div>
        );
    }
}

ReactDOM.render(<Game />, document.getElementById("root"));
