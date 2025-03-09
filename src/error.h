#ifndef ERROR_H
#define ERROR_H
#include <stdexcept>

class Error : public std::runtime_error {
public:
  explicit Error(const std::string& message) : std::runtime_error(message) {}
};

#endif //ERROR_H
