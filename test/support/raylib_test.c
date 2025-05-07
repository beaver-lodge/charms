#include "raylib.h"

int _mlir_ciface_Elixir$RaylibTest$ScreenWidthTest$init_window_wrapper() {
    int width, height;
    if (GetScreenWidth() > 0 && GetScreenHeight() > 0) {
        width = GetScreenWidth();
        height = GetScreenHeight();
    } else {
        width = 800;
        height = 600;
    }
    const char *title = "Test Window";
    TraceLog(LOG_INFO, "Initializing window: %dx%d, title: %s", width, height, title);
    InitWindow(width, height, title);
    TraceLog(LOG_INFO, "Window initialized - IsWindowReady: %d", IsWindowReady());

    if (IsWindowReady()) {
        TraceLog(LOG_INFO, "Entering window loop");
        // Keep window open until ESC pressed
        while (!WindowShouldClose()) {
            TraceLog(LOG_DEBUG, "Window loop iteration");
            BeginDrawing();
            ClearBackground(RAYWHITE);
            DrawText("Test Window Open", 20, 20, 20, DARKGRAY);
            EndDrawing();
        }
    }

    return IsWindowReady();
}

void close_window(void) {
    CloseWindow();
}

// clang test/support/raylib_test.c -o test_window.exe -I/opt/homebrew/include -L/opt/homebrew/lib -lraylib
int main(void) {
    if (_mlir_ciface_Elixir$RaylibTest$ScreenWidthTest$init_window_wrapper()) {
        TraceLog(LOG_INFO, "Window test completed successfully");
    } else {
        TraceLog(LOG_ERROR, "Window initialization failed");
    }
    close_window();
    return 0;
}
