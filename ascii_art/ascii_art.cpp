#include <iostream>
#include <string>
#include <unordered_map>
#include <filesystem>
#include <fstream>

// ASCII ART inspired by:
// ┌─┐┬  ┬┌─┐┌─┐┌┬┐┌─┐┬─┐   Compact SVO optimized vector C++17 or higher
// └─┐└┐┌┘├┤ │   │ │ │├┬┘   Version 1.0.3
// └─┘ └┘ └─┘└─┘ ┴ └─┘┴└─   https://github.com/martinus/svector

const char8_t *svector_style[] = {
    u8" \n"
    u8" \n"
    u8" \n"
    u8" \n", // space

    u8"│\n"
    u8"│\n"
    u8"╷\n", // !

    u8"║\n"
    u8" \n"
    u8" \n", // "

    u8"┼─┼\n"
    u8"┼─┼\n", // #

    u8"┌╫┐\n"
    u8"└╫┐\n"
    u8"└╫┘\n", // $

    u8"╭╮╱\n"
    u8"╰╱╮\n"
    u8"╱╰╯\n", // %

    u8"   \n"
    u8" & \n"
    u8"   \n", // &

    u8"╷\n"
    u8" \n"
    u8" \n", // '

    u8"╭  \n"
    u8"│  \n"
    u8"╰  \n",

    u8"  ╮\n"
    u8"  │\n"
    u8"  ╯\n",

    u8"   \n"
    u8" * \n"
    u8"   \n",

    u8" │ \n"
    u8"─┼─\n"
    u8" │ \n",

    u8"   \n"
    u8"   \n"
    u8" , \n",

    u8"   \n"
    u8"───\n"
    u8"   \n",

    u8"   \n"
    u8"   \n"
    u8" . \n",

    u8"  ╱\n"
    u8" ╱ \n"
    u8"╱  \n",

//
    u8"╭─╮\n"
    u8"│╱│\n"
    u8"╰─╯\n",

    u8"╱│ \n"
    u8" │ \n"
    u8"─┴─\n",

    u8"╭─╮\n"
    u8"╭─┘\n"
    u8"└─┘\n",

    u8"╭─╮\n"
    u8" ─┤\n"
    u8"╰─╯\n",

    u8"┬ ┬\n"
    u8"└─┤\n"
    u8"  ┴\n",

    u8"┌─ \n"
    u8"└─┐\n"
    u8" ─┘\n",

    u8"┌─ \n"
    u8"├─┐\n"
    u8"└─┘\n",

    u8"┌─┐\n"
    u8"  │\n"
    u8"  ┴\n",

    u8"┌─┐\n"
    u8"├─┤\n"
    u8"└─┘\n",

    u8"┌─┐\n"
    u8"└─┤\n"
    u8"  ┴\n",

    u8" ╷ \n"
    u8"   \n"
    u8" ╷ \n", // :

    u8" ╷ \n"
    u8"   \n"
    u8" ╱ \n", // ;

    u8"   \n"
    u8" ╱ \n"
    u8" ╲ \n", // <

    u8"   \n"
    u8"═══\n"
    u8"   \n", // =

    u8"   \n"
    u8" ╲ \n"
    u8" ╱ \n", // >

    u8"╭─╮\n"
    u8" ╭╯\n"
    u8" ╷ \n", // ?

    u8"   \n"
    u8" @ \n"
    u8"   \n", // @

//
    u8"┬─┐\n"
    u8"├─┤\n"
    u8"┴ ┴\n",

    u8"┬─┐\n"
    u8"├─┤\n"
    u8"┴─┘\n",

    u8"┌─┐\n"
    u8"│  \n"
    u8"└─┘\n",

    u8"┬─┐\n"
    u8"│ │\n"
    u8"┴─┘\n",

    u8"┌─┐\n"
    u8"├┤ \n"
    u8"└─┘\n",

    u8"┬──\n"
    u8"├─┤\n"
    u8"┴  \n",

    u8"┌─┐\n"
    u8"│ ┐\n"
    u8"└─┘\n",

    u8"┬ ┬\n"
    u8"├─┤\n"
    u8"┴ ┴\n",

    u8"┬\n"
    u8"│\n"
    u8"┴\n",

    u8"  ┬\n"
    u8"  │\n"
    u8"└─┘\n",

    u8"┬ ╱\n"
    u8"├┤ \n"
    u8"┴ ╲\n",

    u8"|  \n"
    u8"│  \n"
    u8"└─┘\n",

    u8"┬┬┐\n"
    u8"│││\n"
    u8"┴ ┴\n",

    u8"┬─┐\n"
    u8"│ │\n"
    u8"┴ ┴\n",

    u8"┌─┐\n"
    u8"│ │\n"
    u8"└─┘\n",

    u8"┬─┐\n"
    u8"├─┘\n"
    u8"┴  \n",

    u8"┌─┐\n"
    u8"│ │\n"
    u8"└─┴\n",

    u8"┬─┐\n"
    u8"├┬┘\n"
    u8"┴└─\n",

    u8"┌─┐\n"
    u8"└─┐\n"
    u8"└─┘\n",

    u8"┌┬┐\n"
    u8" │ \n"
    u8" ┴ \n",

    u8"┬ ┬\n"
    u8"│ │\n"
    u8"└─┘\n",

    u8"┬  ┬\n"
    u8"└┐┌┘\n"
    u8" └┘ \n",

    u8"┬ ┬\n"
    u8"│││\n"
    u8"┴┴┘\n",

    u8"╲ ╱\n"
    u8" ╳ \n"
    u8"╱ ╲\n",

    u8"┬ ┬\n"
    u8"└┬┘\n"
    u8" ┴ \n",

    u8"──╱\n"
    u8" ╱ \n"
    u8"╱──\n",

    u8"┌  \n"
    u8"│  \n"
    u8"└  \n",

    u8"╲  \n"
    u8" ╲ \n"
    u8"  ╲\n",

    u8"  ┐\n"
    u8"  │\n"
    u8"  ┘\n",

    u8" ╱╲ \n"
    u8"    \n"
    u8"    \n",

    u8"   \n"
    u8"   \n"
    u8"───\n",

    u8"`  \n"
    u8"   \n"
    u8"   \n",
};

struct ascii_art_table {
    //std::vector<std::string> printable;
    std::vector<std::vector<std::string>> char_section{};
    size_t chars_height = 0;
    char start_char = ' ';
    char end_char = '~';

    template<size_t N>
    ascii_art_table(const char8_t* (&art)[N], char low_range, char high_range) {
        size_t max_height = 0;
        for (size_t ptr_idx = 0; ptr_idx < N; ptr_idx++) {
            const uint8_t* multi_line_ptr = (const uint8_t*)art[ptr_idx];
            //std::string_view multi_line_char = multi_line_ptr;
            size_t till_null = 0;
            const size_t limit = 1 << 12;
            for (; till_null < limit && multi_line_ptr && ((const uint8_t*)multi_line_ptr)[till_null]; till_null++);

            size_t previous = 0;
            auto& new_char = char_section.emplace_back();
            for (size_t i = 0; i < till_null; i++) {
                if (((const uint8_t*)multi_line_ptr)[i] == (uint8_t)'\n' || i == (till_null - 1))
                {
                    std::string_view char_row{
                        (const char*)multi_line_ptr + previous,
                        (const char*)multi_line_ptr + i
                    };
                    new_char.emplace_back(char_row);
                    // skip the '\n' or \0
                    previous = i + 1;
                }
            }
            max_height = std::max(max_height, new_char.size());
        }
        chars_height = max_height;
    }

    template<size_t N>
    ascii_art_table(const char*(&art)[N], char low_range, char high_range) {
        size_t max_height = 0;
        for (size_t ptr_idx = 0; ptr_idx < N; ptr_idx++) {
            const uint8_t* multi_line_ptr = (const uint8_t*)art[ptr_idx];
            //std::string_view multi_line_char = multi_line_ptr;
            size_t till_null = 0;
            const size_t limit = 1 << 12;
            for (; till_null < limit && multi_line_ptr && ((const uint8_t*)multi_line_ptr)[till_null]; till_null++);

            size_t previous = 0;
            auto& new_char = char_section.emplace_back();
            for (size_t i = 0; i < till_null; i++) {
                if (((const uint8_t*)multi_line_ptr)[i] == '\n' || i == (till_null - 1))
                {
                    std::string_view char_row{
                        (const char*)multi_line_ptr + previous,
                        (const char*)multi_line_ptr + i
                    };
                    new_char.emplace_back(char_row);
                    // skip the '\n' or \0
                    previous = i + 1;
                }
            }
            max_height = std::max(max_height, new_char.size());
        }
        chars_height = max_height;
    }

    template<typename T>
    ascii_art_table(const std::vector<const T*>& art, char low_range, char high_range) {
        size_t max_height = 0;
        for (const T* multi_line_typed_ptr : art) {
            const uint8_t* multi_line_ptr = (const uint8_t*)multi_line_typed_ptr;
            //std::string_view multi_line_char = multi_line_ptr;
            size_t till_null = 0;
            const size_t limit = 1 << 12;
            for (; till_null < limit && multi_line_ptr && ((const uint8_t*)multi_line_ptr)[till_null]; till_null++);

            size_t previous = 0;
            auto& new_char = char_section.emplace_back();
            for (size_t i = 0; i < till_null; i++) {
                if (((const uint8_t*)multi_line_ptr)[i] == '\n' || i == (till_null - 1))
                {
                    std::string_view char_row{
                        (const char *)multi_line_ptr + previous,
                        (const char *)multi_line_ptr + i
                    };
                    new_char.emplace_back(char_row);
                    // skip the '\n' or \0
                    previous = i + 1;
                }
            }
            max_height = std::max(max_height, new_char.size());
        }
        chars_height = max_height;
    }
    /*
    ascii_art_table(const std::vector<const char *>& art, char low_range, char high_range) {
        size_t max_height = 0;
        for (const char *multi_line_ptr : art) {
            //std::string_view multi_line_char = multi_line_ptr;
            size_t previous = 0;
            auto& new_char = char_section.emplace_back();
            for (size_t i = 0; i < multi_line_char.size(); i++) {
                if (multi_line_char[i] == '\n' ||
                    i == (multi_line_char.size() - 1)) {
                    std::string_view char_row {
                        multi_line_char.data() + previous,
                        multi_line_char.data() + i
                    };
                    new_char.emplace_back(char_row);
                    // skip the '\n' or \0
                    previous = i + 1;
                }
            }
            max_height = std::max(max_height, new_char.size());
        }
        chars_height = max_height;
    }
    */

    ascii_art_table(const std::vector<std::string> &art, char low_range, char high_range) {
        size_t max_height = 0;
        for (const std::string& multi_line_char : art) {
            size_t previous = 0;
            auto &new_char = char_section.emplace_back();
            for (size_t i = 0; i < multi_line_char.size(); i++) {
                if (multi_line_char[i] == '\n' ||
                    i == (multi_line_char.size() - 1)) {
                    new_char.emplace_back(std::string_view{
                        multi_line_char.data()+previous,
                        multi_line_char.data()+i
                    });
                    // skip the '\n' or \0
                    previous = i + 1;
                }
            }
            max_height = std::max(max_height, new_char.size());
        }
        chars_height = max_height;
    }

    std::string print(std::string_view v) {
        std::u8string output;

        size_t previous_idx = 0;
        size_t idx = 0;
        //size_t next_newline = 0;
        while (idx < v.size()) {
            for (size_t row = 0; row < chars_height; row++) {
                size_t max_chars = 0;
                bool found_newline = false;

                for (idx = previous_idx; idx < v.size(); idx++) {
                    char uppered = std::toupper(v[idx]);

                    if (uppered == '\n') {
                        /*
                        if (row == 0) {
                            for (size_t nl = 0; nl < (chars_height + 1); nl++)
                                output.push_back('\n');
                            break;
                        }
                        */
                        found_newline = true;
                        //next_newline = idx;
                        if (row == chars_height - 1) {
                            previous_idx = idx + 1;
                            output.push_back('\n');
                        }
                        break;
                    }

                    if (start_char <= uppered && uppered <= end_char) {
                        int index = (int)(uppered - start_char);
                        std::u8string_view chars_row = u8"";

                        if (row < char_section[index].size()) {
                            std::string_view vw = char_section[index][row];
                            chars_row = std::u8string_view{ (const char8_t*)vw.data(), vw.size() };
                            max_chars = std::max(max_chars, chars_row.size());
                            output.append(chars_row);
                        }
                        else {
                            output.append(max_chars, ' ');
                        }
                    }
                }

                output.push_back('\n');
            }
        }

        std::string raw_out;
        raw_out.assign((const char*)output.data(), output.size());
        return raw_out;
    }
};

int main() {
    std::string input; //= "svector\n\ttest 0123";
    std::cout << "Enter a string: ";
    std::getline(std::cin, input);

    ascii_art_table table{ svector_style, ' ', '`'};
    std::string result = table.print(input);

    std::ofstream out(std::filesystem::current_path() / "output.txt");
    out << input;
    out << "\n\n";
    out << result;

    out.close();

    return 0;
}
