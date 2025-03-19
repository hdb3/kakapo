from datetime import datetime


def string_to_datetime(date_string):
    try:
        # Attempt to parse with microseconds (up to 6 digits)
        return datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S.%f")
    except ValueError:

        try:
            # Handle up to 9 digits of fractional seconds (nanoseconds)
            # by truncating to 6 digits (microseconds) if necessary
            parts = date_string.split(".")
            if len(parts) == 2:
                date_part = parts[0]
                frac_part = parts[1]
                frac_part = frac_part[:6].ljust(6, "0")
                new_date_string = f"{date_part}.{frac_part}"

                return datetime.strptime(new_date_string, "%Y-%m-%d %H:%M:%S.%f")
            else:
                return datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

        except ValueError:
            return None
