#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include "test.h"

int getCell(int x, int y) {
    return func4(x, y);
}

void setCell(int x, int y, int z) {
    func3(x, y, z);
}

void tick() {
    func13();
}

void render() {
    for (int y = 0; y < 3; y++) {
        for (int x = 0; x < 50; x++) {
            if (getCell(x, y) > 0) {
                mvaddch(y+1,x,"0");
            } else {
                mvaddch(y+1,x,"_");
            }
        }
    }
    refresh();
}


int main() {
  printf("start\n");
  _main();
  printf("mem+tables allocated\n");
  for (int i = 0; i < 500; i++) {
    fprintf(stderr, "setting cells\n");
    setCell(rand() % 50, rand() % 50, 1);
  }
  fprintf(stderr, "cells are set\n");
  initscr();
  cbreak();
  noecho();

  for (int i=0; i<10; i++) {
    //   fprintf(stderr, "ticky\n");
      mvaddstr(0,0,"------------------------gen------------------------\n");
      render();
      tick();
    //   fprintf(stderr, "tocky\n");
      sleep(1);
  }
  endwin();
}