import java.beans.* ;
import java.awt.* ;
import java.awt.event.* ;
import javax.swing.* ;
import javax.swing.event.* ;

public class MyJFrame extends JFrame
{
    public MyJFrame(String title) {
        super(title) ;
        setBounds(0, 30, 300, 150) ;
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE) ;
        Container content = getContentPane() ;
        SpringLayout layout = new SpringLayout() ;
        content.setLayout(layout) ;

        final TickTack ticktack = new TickTack() ;
        content.add(ticktack);
 
        final ColoredButton button = new ColoredButton("Click Me!") ;
        button.setBounds(50, 60, 100, 25) ;
        content.add(button);
        SpringLayout.Constraints constr = layout.getConstraints(button) ;
        constr.setConstraint(SpringLayout.WEST, Spring.constant(50)) ;
        constr.setConstraint(SpringLayout.NORTH, Spring.constant(60)) ;

        ticktack.addPropertyChangeListener(new PropertyChangeListener() {
            public void propertyChange(PropertyChangeEvent e) {
                button.setColor(ticktack.getTicks()) ; 
				CxProlog.PostEvent(ticktack.getTicks()) ;
            }
        }) ;

        button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
				CxProlog.PostEvent(button.getColor()) ;
            }
        }) ;

        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
				CxProlog.PostEvent("stop") ;
            }
        }) ;

        setVisible(true) ;
    }
}
