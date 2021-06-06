#include <errno.h>   
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include "test.h"

void render() {
    for (int y = 0; y < 50; y++) {
        mvaddch(y+1,0,'|');
        for (int x = 0; x < 50; x++) {
            if (getCell(x, y) > 0) {
                mvaddch(y+1,x+1,'0');
            } else {
                mvaddch(y+1,x+1,'_');
            }
        }
        mvaddch(y+1,51,'|');
    }
    refresh();
}


int main(int argc, char *argv[]) {
  char *p;
  int seed = 500;
  int gens = 10;
  
  errno = 0;

  if (argc == 2) {
    long conv = strtol(argv[1], &p, 10);
    seed = conv;
    gens = 10;
  } else if (argc == 3) {
    long conv = strtol(argv[1], &p, 10);
    seed = conv;
    conv = strtol(argv[2], &p, 10);
    gens = conv;
  } else {
    seed = 500;
    gens = 10;
  }

  printf("start\n");
  for (int i = 0; i < seed; i++) {
    // fprintf(stderr, "setting cells\n");
    setCell(rand() % 50, rand() % 50, 1);
  }
  fprintf(stderr, "cells are set\n");
  initscr();
  cbreak();
  noecho();

  for (int i=0; i<gens; i++) {
    //   fprintf(stderr, "ticky\n");
      mvaddstr(0,0,"____________________________________________________\n");
      render();
      tick();
    //   fprintf(stderr, "tocky\n");
      sleep(1);
  }
  ___wasmc__drop();
  endwin();
}
