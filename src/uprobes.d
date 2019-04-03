provider julia {
  probe compile__start();
  probe compile__end(char*);
};
