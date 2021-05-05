/**
 * SSG: A simple static site generator
 **/

#include <ctype.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#define ENTRY_FIELD_SIZE 256

typedef struct index {
  char* title;
  char* author;
  char* date;
  char* link;
  struct index* next;
} index_t;

/**
 * Constructing the index.
 **/

char* create_link(char* path, char* dest_dir) {
  char* link;
  asprintf(&link, "%s/%s", dest_dir, path);

  // Replace the old extension with .html
  char* ext = strrchr(link, '.');
  strcpy(ext, ".html");

  return link;
}

index_t* read_entry(char* path, char* src_dir, char* dest_dir) {
  index_t* entry = malloc(sizeof(index_t));
  char* title = malloc(sizeof(char) * ENTRY_FIELD_SIZE);
  char* author = malloc(sizeof(char) * ENTRY_FIELD_SIZE);
  char* date = malloc(sizeof(char) * ENTRY_FIELD_SIZE);

  entry->title = title;
  entry->author = author;
  entry->date = date;
  entry->link = create_link(path, dest_dir);

  // We are looking for two matching lines that start with '-'.
  // Therefore, every time we see one, we are going to decrement the
  // read_lines counter, and terminate once it hits zero.
  int read_lines = 2;
  char line[ENTRY_FIELD_SIZE];

  char* src_path;
  asprintf(&src_path, "%s/%s", src_dir, path);
  FILE* fp = fopen(src_path, "r");

  if (fp == NULL) {
    fprintf(stderr, "Could not open file %s\n", path);
    exit(1);
  }

  while (read_lines && fgets(line, ENTRY_FIELD_SIZE, fp)) {
    line[strcspn(line, "\n")] = 0;

    if (strcmp("date:", line) == 0) {
      strcpy(date, line + 6);
    } else if (strcmp("author:", line) == 0) {
      strcpy(author, line + 8);
    } else if (strcmp("title:", line) == 0) {
      strcpy(title, line + 7);
    } else if (line[0] == '-') {
      read_lines--;
    }
  }

  fclose(fp);
  free(src_path);
  return entry;
}

index_t* read_index(char* src_dir, char* dest_dir) {
  index_t* head;
  index_t* current;
  struct dirent* dir;

  DIR* d = opendir(src_dir);
  if (d == NULL) {
    fprintf(stderr, "Could not open source directory %s\n", src_dir);
    exit(1);
  }

  while((dir = readdir(d)) != NULL) {
    char* ext = strrchr(dir->d_name, '.');
    if (strcmp(ext, ".md") == 0) {
      if (head == NULL) {
        head = read_entry(dir->d_name, src_dir, dest_dir);
        current = head;
      } else {
        current->next = read_entry(dir->d_name, src_dir, dest_dir);
        current= current->next;
      }
    }
  }

  closedir(d);
  return head;
}

/**
 * Rendering the index.
 **/

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

/**
 * Command Line option parsing.
 **/

int main(int argc, char** argv) {
  int opt;
  char* src_dir;
  char* dest_dir;
  char* index_path;

  while ((opt = getopt (argc, argv, "s:d:i:")) != -1) {
    switch (opt) {
    case 's':
      src_dir = optarg;
      break;
    case 'd':
      dest_dir = optarg;
      break;
    case 'i':
      index_path = optarg;
      break;
    default:
      exit(1);
    }
  }

  if (src_dir == NULL) {
    fprintf(stderr, "Please provide a source directory via -s\n");
    exit(1);
  }

  if (dest_dir == NULL) {
    fprintf(stderr, "Please provide a destination directory via -d\n");
    exit(1);
  }

  FILE* index_file = stdout;
  if (index_path != NULL) {
    index_file = fopen(index_path, "w");
  }

  index_t* index = read_index(src_dir, dest_dir);
  render_index(index, index_file);

  if (index_path != NULL) {
    fclose(index_file);
  }

  exit(0);
}
