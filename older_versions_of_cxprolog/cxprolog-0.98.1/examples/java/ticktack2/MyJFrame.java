package ticktack2 ;

import java.beans.* ;
import java.awt.* ;
import java.awt.event.* ;
import javax.swing.* ;
import javax.swing.event.* ;

import prolog.* ;

public class MyJFrame extends JFrame
{
	private TickTack ticktack;
    public MyJFrame(String title) {
        super(title) ;
        setBounds(80, 30, 300, 150) ;
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE) ;
        Container content = getContentPane() ;
        SpringLayout layout = new SpringLayout() ;
        content.setLayout(layout) ;
        ticktack = new TickTack() ;
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
				Prolog.PostEvent("event", MyJFrame.this, null, "tick", ticktack.getTicks()) ;
            }
        }) ;

        button.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
			Prolog.PrintThreadId("GUI") ;
			Prolog.CallProlog("true") ; // TEST FORBIDDEN INTERTHREAD REENTRANCY
			Prolog.PostEvent("event", MyJFrame.this, null, "color", button.getColor()) ;
        }
        }) ;

        this.addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
				Prolog.PostEvent("event", MyJFrame.this, null, "stop") ;
            }
        }) ;

        setVisible(true) ;
		toFront() ;
    }

	public void close()
	{
		ticktack.stop();
		SwingUtilities.invokeLater(
			new Runnable() { public void run() { dispose(); } }
		);
	}

    public static void main(String[] args) {
		if( true ) {
			Prolog.StartProlog();
			Prolog.hello();
			Prolog.CallProlog("writeln", "zzzzz");
			Prolog.CallProlog(",","writeln", "aaa", null, null,
							"writeln", "bbb");
			Prolog.CallProlog("[control]");
			Prolog.CallProlog("listing");
			Prolog.CallProlog("run");
		}
		else
			try {
				java.net.URL fileURL =
					new java.net.URL("file:/home/amd/development/CxProlog/lib/cxprolog/java/prolog.jar");
				java.net.URLClassLoader clsLoader = new java.net.URLClassLoader(new java.net.URL[] {fileURL});
				Class<?> prolog = clsLoader.loadClass("prolog.Prolog");
				prolog.getDeclaredMethod("StartProlog").invoke(null);
				prolog.getDeclaredMethod("CallProlog", String.class).invoke(null, "writeln(ola)");
			} catch( Exception e ) {
				System.out.println(e);
			}
	}
}
