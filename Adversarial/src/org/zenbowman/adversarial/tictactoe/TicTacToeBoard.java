package org.zenbowman.adversarial.tictactoe;


import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.stage.Stage;
import org.zenbowman.adversarial.AlphaBetaSearch;
import scala.Tuple2;

class ResetHandler implements EventHandler<MouseEvent> {
    final TicTacToeBoard board;

    public ResetHandler(TicTacToeBoard ticTacToeBoard) {
        board = ticTacToeBoard;
    }

    @Override
    public void handle(MouseEvent mouseEvent) {
        board.reset();
    }
}

class ClickHandler implements EventHandler<MouseEvent> {
    final int x;
    final int y;
    final TicTacToeBoard board;

    public ClickHandler(int x, int y, TicTacToeBoard board) {
        this.x = x;
        this.y = y;
        this.board = board;
    }

    @Override
    public void handle(MouseEvent mouseEvent) {
        System.out.println(String.format("Clicked: (%s, %s)", x, y));
        board.playPlayerTurn(x, y);
    }
}

public class TicTacToeBoard extends Application {
    final Rectangle[] rectangles = new Rectangle[9];
    final AlphaBetaSearch<TicTacToeState> alphaBetaSearch = TicTacToeHelper.gamePlayer();
    TicTacToeState currentState;

    public int rectIndex(int x, int y) {
        return x + (3 * y);
    }

    public void updateRectangle(int x, int y, int turn) {
        final int rectIndex = rectIndex(x, y);
        final Rectangle thisRect = rectangles[rectIndex];
        if (turn == 1) {
            thisRect.setFill(Color.RED);
        } else if (turn == -1) {
            thisRect.setFill(Color.BLUE);
        } else {
            thisRect.setFill(Color.WHITE);
        }
    }

    public Rectangle createRectangleForPosition(int x, int y) {
        final int rectIndex = rectIndex(x, y);
        final Rectangle thisRect = new Rectangle();
        thisRect.setX((x * 50) + 10);
        thisRect.setY((y * 50) + 10);
        thisRect.setWidth(50);
        thisRect.setHeight(50);
        thisRect.setFill(Color.WHITE);
        thisRect.setStroke(Color.BLACK);
        thisRect.setStrokeWidth(3);
        thisRect.setOnMouseClicked(new ClickHandler(x, y, this));
        rectangles[rectIndex] = thisRect;
        return thisRect;
    }

    public void reset() {
        currentState = TicTacToeHelper.initialState();

        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                updateRectangle(i, j, 0);
            }
        }

        //playComputerTurn();
    }

    public void playComputerTurn() {
        final TicTacToeAction ticTacToeAction = (TicTacToeAction) alphaBetaSearch.decision(currentState);
        currentState = currentState.stateWithAction(1, ticTacToeAction.position());
        final Tuple2<Integer, Integer> position = (Tuple2<Integer, Integer>) ticTacToeAction.position();
        updateRectangle(position._1(), position._2(), 1);
    }

    public void playPlayerTurn(int x, int y) {
        if (currentState.turn() == -1) {
            currentState = currentState.stateWithAction(-1, x, y);
            updateRectangle(x, y, -1);
        }
        if (currentState.availableSpots().nonEmpty()) {
            playComputerTurn();
        }
    }


    @Override
    public void start(Stage stage) throws Exception {
        final Group root = new Group();
        final Scene scene = new Scene(root, 180, 280);

        currentState = TicTacToeHelper.initialState();

        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                root.getChildren().add(createRectangleForPosition(i, j));
            }
        }
        final Group resetButtonGroup = new Group();
        resetButtonGroup.setTranslateX(60);
        resetButtonGroup.setTranslateY(200);
        final Button resetButton = new Button("Reset");
        resetButtonGroup.getChildren().add(resetButton);

        root.getChildren().add(resetButtonGroup);
        resetButton.setOnMouseClicked(new ResetHandler(this));

        stage.setTitle("TIC TAC TOE");
        stage.setScene(scene);
        stage.show();

        //playComputerTurn();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
