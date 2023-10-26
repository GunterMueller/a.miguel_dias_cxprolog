package ticktack ;

import java.beans.* ;
import java.awt.* ;
import java.awt.event.* ;
import javax.swing.* ;
import javax.swing.event.* ;

import prolog.* ;

public class ColoredButton extends JButton
{
	private int color = 0 ;

    public ColoredButton() {
        this("ColoredButton") ;
    }

    public ColoredButton(String title) {
        super(title) ;
    }

    public String getColor() {
        switch( color ) {
            case 0: return "red" ;
            case 1: return "blue" ;
            case 2: return "green" ;
            case 3: return "yellow" ;
			default: return "unknown" ;
        }
    }

    public void setColor(int t) {
		color = t % 4 ;
        switch( color ) {
            case 0: setBackground(Color.red) ; break ;
            case 1: setBackground(Color.blue) ; break ;
            case 2: setBackground(Color.green) ; break ;
            case 3: setBackground(Color.yellow) ; break ;
			default: break ;
        }
    }
}
