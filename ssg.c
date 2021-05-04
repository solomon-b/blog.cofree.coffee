// A simple static site generator

#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ENTRY_FIELD_SIZE 256


typedef struct index {
  char* title;
  char* author;
  char* date;
  char* link;
  struct index* next;
} index_t;

char* create_link(char* path) {
  char* link = malloc(sizeof(char) * ENTRY_FIELD_SIZE);
  char* ext;

  strcpy(link, path);
  ext = strrchr(link, '.');
  strcpy(ext, ".html");
  return link;
}

index_t* read_entry(char* path) {
  index_t* entry = malloc(sizeof(index_t));
  char* title = malloc(sizeof(char) * ENTRY_FIELD_SIZE);
  char* author = malloc(sizeof(char) * ENTRY_FIELD_SIZE);
  char* date = malloc(sizeof(char) * ENTRY_FIELD_SIZE);

  entry->title = title;
  entry->author = author;
  entry->date = date;
  entry->link = create_link(path);

  // We are looking for two matching lines that start with '-'.
  // Therefore, every time we see one, we are going to decrement the
  // read_lines counter, and terminate once it hits zero.
  int read_lines = 2;
  char line[256];
  FILE* fp = fopen(path, "r");
  while (read_lines && fgets(line, ENTRY_FIELD_SIZE, fp)) {
    line[strcspn(line, "\n")] = 0;
    switch (line[0]) {
    case '-':
      read_lines--;
      break;
    case 't':
      strcpy(title, line + 7);
      break;
    case 'a':
      strcpy(author, line + 8);
      break;
    case 'd':
      strcpy(date, line + 6);
      break;
    }
  }

  fclose(fp);
  return entry;
}

index_t* read_index(char* path) {
  index_t* index;
  struct dirent* dir;
  DIR* d = opendir(path);
  char *fpath;

  while((dir = readdir(d)) != NULL) {
    char* ext = strrchr(dir->d_name, '.');
    if (strcmp(ext, ".md") == 0) {
      asprintf(&fpath, "%s/%s", path, dir->d_name);
      printf("Adding %s\n", fpath);
      if (index == NULL) {
        index = read_entry(fpath);
      } else {
        index->next = read_entry(fpath);
        index = index->next;
      }
    }
  }

  closedir(d);
  return index;
}

void render_index(index_t* index, FILE* fp) {
  index_t* item = index;
  fprintf(fp, "<!doctype html>\n");
  fprintf(fp, "<html lang=\"en\">\n");
  fprintf(fp, "<head>\n");
  fprintf(fp, "  <meta charset=\"utf-8\">\n");
  fprintf(fp, "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n");
  fprintf(fp, "  <meta name=\"description\" content=\"blog.cofree.coffee\" />\n");
  fprintf(fp, "  <title>blog.cofree.coffee</title>\n");
  fprintf(fp, "  \n");
  fprintf(fp, "</head>\n");
  fprintf(fp, "<body>\n");
  fprintf(fp, "  <div id=\"runner1\">SOLOMON'S BLOG</div>\n");
  fprintf(fp, "  <div id=\"runner1b\">functional programming, permaculture, math</div>\n");
  fprintf(fp, "  <ul>\n");

  while(item != NULL) {
    fprintf(fp, "      <li><a href=\"%s\">%s<span>%s</span></a></li>\n", item->link, item->title, item->date);
    item = item->next;
  }

  fprintf(fp, "  </ul>\n");
  fprintf(fp, "</body>\n");
  fprintf(fp, "</html>\n");
}

int main(int argc, char** argv) {
  if (argc != 2) {
    fprintf(stderr, "%s", "Please provide the directory you want to generate an index file for.");
    exit(1);
  }

  char* dir = argv[1];

  index_t* index = read_index(dir);
  render_index(index, stdout);
  return 0;
}
