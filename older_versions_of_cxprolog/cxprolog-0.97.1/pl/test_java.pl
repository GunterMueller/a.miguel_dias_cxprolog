#!/usr/local/bin/cxprolog --script

/* Unit tests for Java */

test :-
java_call('javax/swing/JFrame', '<init>:(Ljava/lang/String;)V', ['Wait 1 second'], Frame),
java_call(Frame, 'setBounds:(IIII)V', [0, 30, 300, 150], _),
java_field('javax/swing/WindowConstants', 'DISPOSE_ON_CLOSE:I', CloseOp, CloseOp),
java_call(Frame, 'setDefaultCloseOperation:(I)V', [CloseOp], _),
java_call(Frame, 'getContentPane:()Ljava/awt/Container;', [], Content),
java_call('javax/swing/SpringLayout', '<init>:()V', [], Layout),
java_call(Content, 'setLayout:(Ljava/awt/LayoutManager;)V', [Layout], _),

java_call('javax/swing/JButton', '<init>:(Ljava/lang/String;)V', ['Wait 1 second'], Button),
java_call(Button, 'setBounds:(IIII)V', [50, 60, 100, 25], _),
java_call(Content, 'add:(Ljava/awt/Component;)Ljava/awt/Component;', [Button], _),

java_call(Layout, 'getConstraints:(Ljava/awt/Component;)Ljavax/swing/SpringLayout$Constraints;', [Button], Constraints),
java_field('javax/swing/SpringLayout', 'WEST:Ljava/lang/String;', West, West),
java_call('javax/swing/Spring', 'constant:(I)Ljavax/swing/Spring;', [50], Spring50),
java_call(Constraints, 'setConstraint:(Ljava/lang/String;Ljavax/swing/Spring;)V', [West, Spring50], _),
java_field('javax/swing/SpringLayout', 'NORTH:Ljava/lang/String;', North, North),
java_call('javax/swing/Spring', 'constant:(I)Ljavax/swing/Spring;', [60], Spring60),
java_call(Constraints, 'setConstraint:(Ljava/lang/String;Ljavax/swing/Spring;)V', [North, Spring60], _),

java_call(Frame, 'setVisible:(Z)V', [true], _),
os_sleep(1),
java_call(Frame, 'dispose:()V', [], _).


:- test
::: true.

:- writeln(done), exit_script.
