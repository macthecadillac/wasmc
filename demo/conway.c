#include <stdio.h>
#include <stdlib.h>
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
    for (int y = 0; y < 30; y++) {
        for (int x = 0; x < 50; x++) {
            if (getCell(x, y) > 0) {
                printf("0");
            } else {
                printf("1");
            }
        }
        printf("\n");
    }
}


int main() {
  printf("start\n");
  _main();
  printf("mem+tables allocated\n");
  for (int i = 0; i < 400; i++) {
    setCell(rand() % 50, rand() % 50, 1);
  }
  printf("cells are set");

  for (int i=0; i<2; i++) {
      printf("tick");
      tick();
      render();
      sleep(2);
  }
}