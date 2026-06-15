#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#define NUM_BUTTONS 9

GtkWidget *buttons[NUM_BUTTONS];
char *button_labels[NUM_BUTTONS];
GtkClipboard *clipboard;
GtkWidget *window;

// Wartet genau ms Millisekunden
void wait_ms(int ms) {
    GTimer *timer = g_timer_new();
    while (g_timer_elapsed(timer, NULL) * 1000 < ms) {
        g_main_context_iteration(NULL, FALSE);
    }
    g_timer_destroy(timer);
}

// Kopiert Text und beendet DANN das Programm
void copy_and_exit(const char *text) {
    if (text && strlen(text) > 0) {
        gtk_clipboard_set_text(clipboard, text, -1);
        gtk_clipboard_store(clipboard);
        g_print("Kopiert: '%s'\n", text);
    }
    
    // WICHTIG: Warte 200ms für parcellite
    wait_ms(450);
    
    gtk_widget_destroy(window);
}

// Button-Klick Handler
void on_button_clicked(GtkWidget *widget, gpointer data) {
    int index = GPOINTER_TO_INT(data);
    if (index >= 0 && index < NUM_BUTTONS) {
        copy_and_exit(button_labels[index]);
    }
}

// Tastatur Handler
gboolean on_key_pressed(GtkWidget *window, GdkEventKey *event, gpointer user_data) {
    guint keyval = event->keyval;
    
    if (keyval == GDK_KEY_Escape) {
        gtk_widget_destroy(window);
        return TRUE;
    }
    
    if ((keyval >= GDK_KEY_1 && keyval <= GDK_KEY_9) || 
        (keyval >= GDK_KEY_KP_1 && keyval <= GDK_KEY_KP_9)) {
        
        int index = -1;
        if (keyval >= GDK_KEY_1 && keyval <= GDK_KEY_9) {
            index = keyval - GDK_KEY_1;
        } else if (keyval >= GDK_KEY_KP_1 && keyval <= GDK_KEY_KP_9) {
            index = keyval - GDK_KEY_KP_1;
        }
        
        if (index >= 0 && index < NUM_BUTTONS) {
            copy_and_exit(button_labels[index]);
            return TRUE;
        }
    }
    
    return FALSE;
}

// Lädt Labels aus Datei
void load_labels_from_file(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        for (int i = 0; i < NUM_BUTTONS; i++) {
            button_labels[i] = g_strdup_printf("Button %d", i + 1);
        }
        return;
    }

    char line[2048];
    int i = 0;
    while (i < NUM_BUTTONS && fgets(line, sizeof(line), file)) {
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') line[len-1] = '\0';
        if (len > 1 && line[len-2] == '\r') line[len-2] = '\0';
        button_labels[i] = g_strdup(line[0] == '\0' ? "Leer" : line);
        i++;
    }
    fclose(file);

    for (; i < NUM_BUTTONS; i++) {
        button_labels[i] = g_strdup_printf("Button %d", i + 1);
    }
}

int main(int argc, char *argv[]) {
    gtk_init(&argc, &argv);
    
    // Labels initialisieren
    for (int i = 0; i < NUM_BUTTONS; i++) {
        button_labels[i] = g_strdup_printf("Button %d", i + 1);
        buttons[i] = NULL;
    }

    // Dateiargument verarbeiten
    if (argc == 2) {
        load_labels_from_file(argv[1]);
    } else if (argc > 2) {
        g_printerr("Fehler: Zu viele Argumente.\n");
        g_printerr("Nutzung: %s [datei.txt]\n", argv[0]);
        return 1;
    }

    // Fenster erstellen
    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "copier_buttons");
    gtk_window_set_role(GTK_WINDOW(window), "copier_buttons");  // WICHTIG für i3
    gtk_window_set_default_size(GTK_WINDOW(window), 450, 450);
    gtk_window_set_position(GTK_WINDOW(window), GTK_WIN_POS_CENTER);
    gtk_window_set_resizable(GTK_WINDOW(window), TRUE);
    
    g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
    g_signal_connect(window, "key-press-event", G_CALLBACK(on_key_pressed), NULL);

    // Dark Theme CSS
    GtkCssProvider *provider = gtk_css_provider_new();
    gtk_css_provider_load_from_data(provider,
        "window { background-color: #1a1a1a; color: #ffffff; }"
        "button { background-color: #2d2d2d; color: #e0e0e0; border: none; "
        "border-radius: 6px; font-size: 40px; font-weight: bold; "
        "padding: 12px; margin: 4px; }"
        "button:hover { background-color: #3a3a3a; color: #ffffff; }"
        "button:active { background-color: #4a4a4a; }"
        "button:focus { box-shadow: 0 0 0 2px #5555ff; }",
        -1, NULL);
    
    GdkDisplay *display = gdk_display_get_default();
    GdkScreen *screen = gdk_display_get_default_screen(display);
    gtk_style_context_add_provider_for_screen(screen,
        GTK_STYLE_PROVIDER(provider),
        GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
    
    g_object_unref(provider);
    
    GtkSettings *settings = gtk_settings_get_default();
    g_object_set(settings, "gtk-application-prefer-dark-theme", TRUE, NULL);

    // Zwischenablage
    clipboard = gtk_clipboard_get(GDK_SELECTION_CLIPBOARD);
    if (!clipboard) {
        g_printerr("Keine Zwischenablage verfügbar!\n");
        return 1;
    }

    // Grid
    GtkWidget *grid = gtk_grid_new();
    gtk_grid_set_row_homogeneous(GTK_GRID(grid), TRUE);
    gtk_grid_set_column_homogeneous(GTK_GRID(grid), TRUE);
    gtk_container_set_border_width(GTK_CONTAINER(grid), 10);
    gtk_container_add(GTK_CONTAINER(window), grid);

    // Buttons
    for (int row = 0; row < 3; row++) {
        for (int col = 0; col < 3; col++) {
            int index = row * 3 + col;
            GtkWidget *button = gtk_button_new_with_label(button_labels[index]);
            
            gtk_widget_set_hexpand(button, TRUE);
            gtk_widget_set_vexpand(button, TRUE);
            gtk_widget_set_margin_start(button, 4);
            gtk_widget_set_margin_end(button, 4);
            gtk_widget_set_margin_top(button, 4);
            gtk_widget_set_margin_bottom(button, 4);
            
            g_signal_connect(button, "clicked", G_CALLBACK(on_button_clicked), GINT_TO_POINTER(index));
            gtk_grid_attach(GTK_GRID(grid), button, col, row, 1, 1);
            buttons[index] = button;
        }
    }

    gtk_widget_show_all(window);
    gtk_main();

    // Speicher freigeben
    for (int i = 0; i < NUM_BUTTONS; i++) {
        g_free(button_labels[i]);
    }

    return 0;
}