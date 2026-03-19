package ticktack3;

import java.beans.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

import prolog.*;

public class MyJFrame extends JFrame
{
	private TickTack ticktack;
	public MyJFrame(String title) {
		super(title);
		setBounds(80, 30, 300, 150);
		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		Container content = getContentPane();
		SpringLayout layout = new SpringLayout();
		content.setLayout(layout);
		ticktack = new TickTack();
		content.add(ticktack);
 
		final ColoredButton button = new ColoredButton("Click Me!");
		button.setBounds(50, 60, 100, 25);
		content.add(button);
		SpringLayout.Constraints constr = layout.getConstraints(button);
		constr.setConstraint(SpringLayout.WEST, Spring.constant(50));
		constr.setConstraint(SpringLayout.NORTH, Spring.constant(60));
		
		final ColoredButton buttonNL = new ColoredButton(".");
		buttonNL.setBounds(150, 60, 100, 25);
		content.add(buttonNL);
		SpringLayout.Constraints constrNL = layout.getConstraints(buttonNL);
		constrNL.setConstraint(SpringLayout.WEST, Spring.constant(150));
		constrNL.setConstraint(SpringLayout.NORTH, Spring.constant(60));

		ticktack.addPropertyChangeListener(new PropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent e) {
			//	System.out.println("event -> tick");
				button.setColor(ticktack.getTicks());
			}
		});

		button.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
			//	System.out.println("event -> " + button.getColor());
				Prolog.coroutiningInputLine("mythread", "writeln(\nuser\n,\nzzz\n)");
			//	Prolog.coroutiningInputLine("mythread", "streams.\n");
			//	Prolog.coroutiningInputLine("mythread", "between(0,10000,X).\n");
			//	Prolog.coroutiningInputLine("mythread", "write(user_error, 'FGFDSGFDSG').\n");
			}
		});

		buttonNL.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Prolog.coroutiningInputLine("mythread", ".\n");
			}
		});

		this.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
			//	System.out.println("event -> close");
				close();
			}
		});

		setVisible(true);
		toFront();
		Prolog.coroutiningStart();
	}

	public void close()
	{
		ticktack.stop();
		Prolog.coroutiningStop();
		SwingUtilities.invokeLater(()->{ dispose(); });
	}
	
	public static void startApp() {
		Prolog.StartProlog();
		Prolog.PrintThreadId("MAIN");
	//	Prolog.CallProlog("mythread :- between(0,10000,X), write(X), write('.'), get_code(Y), writeln(Y), os_sleep(0.5), fail.");
	//	Prolog.CallProlog("mythread :- between(0,10000,X), write(X), write('.'), read(Y), writeln(Y), fail.");
		Prolog.CallProlog("mythread :- '$top_level2'.");
		Prolog.CallProlog("thread_new(mythread,mythread,halt)");
		new MyJFrame("ola");
	}

	public static void main(String[] args) {
		Prolog.GuiCall(()->{ startApp(); });
	/*
			try {
				java.net.URL fileURL =
					new java.net.URL("file:/home/amd/development/CxProlog/lib/cxprolog/java/prolog.jar");
				java.net.URLClassLoader clsLoader = new java.net.URLClassLoader(new java.net.URL[] {fileURL});
				Class<?> prolog = clsLoader.loadClass("prolog.Prolog");
				prolog.getDeclaredMethod("StartProlog").invoke(null);
				prolog.getDeclaredMethod("CallProlog", String.class).invoke(null, "writeln(ola)");
			} catch( Exception e ) {
				System.out.println(e);
			}*/
	}
}
