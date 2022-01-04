import React from "react";
import ReactDOM from "react-dom";
import "./index.css";
import words5letters from "./assets/words5.txt";

// read data and load it into a list of words WORDS

let WORDS = [];
let SECRET_WORD = "";
const MAX_GUESSES = 7;
const WORD_LENGTH = 5;
// Game of Wordle: given a secret word, the user must guess the word

// Square class
function Square(props) {
    return (
        <div
            className="square"
            onKeyDown={props.onKeyDown}
            style={{ backgroundColor: props.color }}
        >
            {props.value}
        </div>
    );
}

// Grid class of MAX_GUESSES lines and 5 columns
class Grid extends React.Component {
    renderSquare(i, j, color) {
        const letter = this.props.guessedLetters[i][j];
        return (
            <Square
                onKeyDown={this.props.onKeyDown}
                key={i * this.props.size + j}
                value={letter ? letter : " "}
                color={color}
            />
        );
    }

    render() {
        // Grid of "size" columns and MAX_GUESSES rows
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
            secretWord: SECRET_WORD,
            guessedLetters: [[], [], [], [], [], [], []],
            colors: Array(MAX_GUESSES)
                .fill(null)
                .map(() => Array(WORD_LENGTH).fill("transparent")),
            numberOfGuesses: 0,
            gameStatus: "playing",
        };
    }

    async componentDidMount() {
        // Event listener for user input
        document.addEventListener(
            "keydown",
            (event) => this.handleKeyDown(event)
            // false
        );
        // fetch words from text file and save them in uppercase
        WORDS = await fetch(words5letters)
            .then((data) => data.text())
            .then((text) => {
                return text.split("\n").map((x) => x.toUpperCase());
            });
        // set the word for current game
        this.setWord();
    }

    getLettersColors() {
        const lastWordColors =
            this.state.colors[this.state.numberOfGuesses].slice();
        const secretWord = this.state.secretWord.split("");
        const guessedLetters =
            this.state.guessedLetters[this.state.numberOfGuesses].slice();

        // Letters in the right place first
        for (let index = 0; index < guessedLetters.length; index++) {
            if (guessedLetters[index] === secretWord[index]) {
                lastWordColors[index] = "rgba(100, 200, 160, 1)";
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
                lastWordColors[index] = "rgba(230, 170, 120, 1)";
                // replace letter already found with a placeholder
                secretWord[secretWord.indexOf(guessedLetter)] = "_";
            }
        }

        const newColors = this.state.colors.slice();
        newColors[this.state.numberOfGuesses] = lastWordColors;
        return newColors;
    }

    handleUserWord() {
        // Color every letter of the word green if it is correctly placed and yellow if it is in the word but not in the right place
        const newColors = this.getLettersColors();
        if (
            this.state.guessedLetters[this.state.numberOfGuesses].join("") ===
            this.state.secretWord
        ) {
            // Check if the word is correct
            this.setState({
                gameStatus: "won",
                colors: newColors,
                numberOfGuesses: this.state.numberOfGuesses + 1,
            });
            return;
        } else if (this.state.numberOfGuesses === MAX_GUESSES - 1) {
            // Check if too many guesses
            this.setState({
                gameStatus: "lost",
                colors: newColors,
                numberOfGuesses: this.state.numberOfGuesses + 1,
            });
        } else {
            // Continue the game otherwise
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
        const afterGuess = this.state.guessedLetters[
            this.state.numberOfGuesses
        ].concat(event.key.toUpperCase());
        newGuessedLetters[this.state.numberOfGuesses] = afterGuess;
        this.setState({
            guessedLetters: newGuessedLetters,
        });
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
        // Select a random word
        SECRET_WORD = WORDS[Math.floor(Math.random() * WORDS.length)];
        console.log(SECRET_WORD);
        // Initialize the game
        this.setState({
            secretWord: SECRET_WORD,
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
                <div>
                    <div className="game-info">
                        <div className="game-info-cell">
                            {this.renderResult()}
                        </div>
                        <div className="game-info-cell">
                            {this.state.numberOfGuesses}/{MAX_GUESSES}
                        </div>
                        <div className="game-info-cell">
                            <button onClick={() => this.setWord()}>
                                Play another word !
                            </button>
                        </div>
                    </div>
                    <div className="game-word"></div>
                </div>
            </div>
        );
    }
}

ReactDOM.render(<Game />, document.getElementById("root"));
