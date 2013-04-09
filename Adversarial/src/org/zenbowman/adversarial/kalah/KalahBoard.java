package org.zenbowman.adversarial.kalah;


import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import java.util.List;


public class KalahBoard extends Application {
    final static String UTILITY_HEADER = "Utility (for BLUE):";
    final static String TERMINAL_HEADER = "is-terminal?";
    final static int COMPUTER_TURN = 2;

    KalahState currentState;
    Text turnText;
    Text utilityText;
    Text terminalText;
    final Rectangle[] rectangles = new Rectangle[14];
    final Text[] textBoxes = new Text[14];


    class HouseClickHandler implements EventHandler<MouseEvent> {
        final int position;
        final KalahBoard board;

        public HouseClickHandler(int position, KalahBoard board) {
            this.position = position;
            this.board = board;
        }

        @Override
        public void handle(MouseEvent mouseEvent) {
            board.handleHouseClick(position);
        }
    }

    void playComputerTurn() {
        System.out.println("Computer is thinking....");
        List<Integer> actions = Kalah.play(currentState).movesAsIntegerList();
        for (Integer positionToplay : actions) {
            playTurn(positionToplay);
            try {
                Thread.sleep(1000);
            } catch (Exception ex) {
            }
            System.out.println(positionToplay);
        }
    }

    void playTurn(int position) {
        try {
            currentState = Kalah.nextStateForMove(currentState, position);
            drawState(currentState);
        } catch (Exception ex) {
            ex.printStackTrace();
            System.out.println("Invalid move");
        }
    }

    void handleHouseClick(int position) {
        playTurn(position);
        if (currentState.whoseTurn() == COMPUTER_TURN) {
            Thread turn = new Thread() {
                public void run() {
                    playComputerTurn();
                }
            };
            turn.start();
        }
    }

    private Rectangle createRectangleAt(int x, int y, Color color) {
        final Rectangle thisRect = new Rectangle();
        thisRect.setX(x);
        thisRect.setY(y);
        thisRect.setWidth(40);
        thisRect.setHeight(40);
        thisRect.setFill(Color.WHITE);
        thisRect.setStroke(color);
        thisRect.setStrokeWidth(3);
        return thisRect;
    }


    private Rectangle createRectangleForPosition(final int position) {
        if (position == 6) {
            rectangles[position] = createRectangleAt(400, 150, Color.RED);
        } else if (position == 13) {
            rectangles[position] = createRectangleAt(50, 150, Color.NAVY);
        } else if (position < 6) {
            rectangles[position] = createRectangleAt((position * 50) + 100, 175, Color.MAROON);
        } else {
            rectangles[position] = createRectangleAt(350 - ((position - 7) * 50), 125, Color.BLUE);
        }
        rectangles[position].setOnMouseEntered(new EventHandler<MouseEvent>() {
            @Override
            public void handle(MouseEvent mouseEvent) {
                rectangles[position].setFill(Color.GRAY);
            }
        });
        rectangles[position].setOnMouseExited(new EventHandler<MouseEvent>() {
            @Override
            public void handle(MouseEvent mouseEvent) {
                rectangles[position].setFill(Color.WHITE);
            }
        });
        rectangles[position].setOnMouseClicked(new HouseClickHandler(position, this));
        return rectangles[position];
    }

    private Text createTextBoxForPosition(int position) {
        final Rectangle positionRect = rectangles[position];
        final Text positionText = new Text();
        positionText.setX(positionRect.getX() + 12);
        positionText.setY(positionRect.getY() + 20);
        positionText.setFont(new Font(16));
        positionText.setText("0");
        textBoxes[position] = positionText;
        return positionText;
    }

    void drawTurn(int whoseTurn) {
        if (whoseTurn == 1) {
            turnText.setText("Turn: Player 1 [RED]");
        } else {
            turnText.setText("Turn: Player 2 [BLUE]");
        }
    }

    void drawUtility(KalahState state) {
        utilityText.setText(String.format("%s %s", UTILITY_HEADER, state.getUtility()));
        terminalText.setText(String.format("%s %s", TERMINAL_HEADER, KalahGame.isTerminal(state)));
    }

    public void drawState(KalahState state) {
        drawTurn(state.whoseTurn());
        drawUtility(state);
        for (int i = 0; i < 14; i++) {
            textBoxes[i].setText(String.valueOf(state.numSeeds(i)));
        }
    }

    Text createText(Group root, int yPosition, String textString) {
        Text text = new Text(20, yPosition, textString);
        text.setFont(new Font(16));
        root.getChildren().add(text);
        return text;
    }

    @Override
    public void start(Stage stage) throws Exception {
        final Group root = new Group();
        final Scene scene = new Scene(root, 500, 250);

        turnText = createText(root, 20, "Turn");
        utilityText = createText(root, 40, UTILITY_HEADER);
        terminalText = createText(root, 60, TERMINAL_HEADER);

        currentState = Kalah.initialState();
        for (int i = 0; i < 14; i++) {
            root.getChildren().add(createRectangleForPosition(i));
            root.getChildren().add(createTextBoxForPosition(i));
        }

        drawState(currentState);

        stage.setTitle("Kalah");
        stage.setScene(scene);
        stage.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
