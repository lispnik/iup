
#include <iup.h>
#include <stdlib.h>
#include <stdio.h>

Ihandle *test1() {
  Ihandle *button, *hbox, *dialog;
  button  = IupButton("Press Me!", NULL);
  hbox = IupHbox(button, NULL);
  dialog = IupDialog(hbox);
  return dialog;
}

Ihandle *test2() {
  Ihandle *button, *hbox, *dialog;
  button  = IupCreatev("button", NULL);
  IupSetStrAttribute(button, "TITLE", "Press Me Too");
  hbox = IupCreatev("hbox", NULL);
  IupAppend(hbox, button);
  dialog = IupCreatev("dialog", NULL);
  IupAppend(dialog, hbox);
  return dialog;
}

int main1 (int argc, char **argv) {
  IupOpen(&argc, &argv);
  /* Ihandle *dialog = test1(); */
  Ihandle *dialog = test2();
  IupShow(dialog);
  IupMainLoop();
}


int close_cb(Ihandle *handle) {
  printf("%p", handle);
  IupExitLoop();
  return IUP_IGNORE;
}

int main2 (int argc, char **argv) {
  IupOpen(&argc, &argv);
  /* Ihandle *dialog = test1(); */
  Ihandle *dialog = test2();
  IupSetCallback(dialog, "CLOSE_CB", close_cb);
  IupShow(dialog);
  IupMainLoop();
}

int main (int argc, char **argv) {
  main2(argc, argv);
}

// gcc -I$HOME/.local/iup-3.25_Linux44_64_lib/include/ -L$HOME/.local/iup-3.25_Linux44_64_lib/ $(pkg-config --libs gtk+-3.0 gdk-3.0)  test.c -liup -liupimglib
