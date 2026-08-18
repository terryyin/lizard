import unittest
from .php_helpers import get_php_function_list


class TestPHPModernFeatures(unittest.TestCase):

    def test_foreach_not_function_and_detects_real_functions(self):
        php_code = '''<?php
class Note {
    public string $text;
    public string $createdAt;

    public function __construct(string $text) {
        $this->text = $text;
        $this->createdAt = date('Y-m-d H:i:s');
    }

    public function toArray(): array {
        return ['text' => $this->text, 'createdAt' => $this->createdAt];
    }

    public static function fromArray(array $data): Note {
        $note = new Note($data['text']);
        $note->createdAt = $data['createdAt'];
        return $note;
    }
}

class NotebookApp {
    private array $notes = [];
    private string $dataFile = 'notes.json';

    public function __construct() {
        $this->loadNotes();
    }

    public function run(): void {
        while (true) {
            $this->printMenu();
            $choice = trim(fgets(STDIN));
            match ($choice) {
                '1' => $this->addNote(),
                '2' => $this->listNotes(),
                '3' => $this->deleteNote(),
                '4' => $this->exitApp(),
                default => print("Invalid choice. Try again.\\n")
            };
        }
    }

    private function printMenu(): void {
        echo "\\n== Notebook CLI App ==\\n";
        echo "1. Add Note\\n";
        echo "2. List Notes\\n";
        echo "3. Delete Note\\n";
        echo "4. Exit\\n";
        echo "Choose an option: ";
    }

    private function addNote(): void {
        echo "Enter your note: ";
        $text = trim(fgets(STDIN));
        if ($text !== '') {
            $note = new Note($text);
            $this->notes[] = $note;
            $this->saveNotes();
            echo "Note added!\\n";
        } else {
            echo "Empty note. Try again.\\n";
        }
    }

    private function listNotes(): void {
        if (empty($this->notes)) {
            echo "No notes available.\\n";
            return;
        }
        echo "\\n== Your Notes ==\\n";
        foreach ($this->notes as $index => $note) {
            echo "[" . ($index + 1) . "] {$note->text} ({$note->createdAt})\\n";
        }
    }

    private function deleteNote(): void {
        $this->listNotes();
        if (empty($this->notes)) return;

        echo "Enter note number to delete: ";
        $input = trim(fgets(STDIN));
        $index = (int)$input - 1;

        if (isset($this->notes[$index])) {
            unset($this->notes[$index]);
            $this->notes = array_values($this->notes); // reindex
            $this->saveNotes();
            echo "Note deleted.\\n";
        } else {
            echo "Invalid note number.\\n";
        }
    }

    private function exitApp(): void {
        echo "Saving and exiting... 👋\\n";
        $this->saveNotes();
        exit(0);
    }

    private function saveNotes(): void {
        $encoded = json_encode(array_map(fn($n) => $n->toArray(), $this->notes), JSON_PRETTY_PRINT);
        file_put_contents($this->dataFile, $encoded);
    }

    private function loadNotes(): void {
        if (file_exists($this->dataFile)) {
            $raw = file_get_contents($this->dataFile);
            $data = json_decode($raw, true);
            $this->notes = array_map(fn($n) => Note::fromArray($n), $data);
        }
    }
}
?>'''
        functions = get_php_function_list(php_code)
        function_names = sorted(f.name for f in functions)

        self.assertIn('Note::__construct', function_names)
        self.assertIn('Note::toArray', function_names)
        self.assertIn('Note::fromArray', function_names)
        self.assertIn('NotebookApp::__construct', function_names)
        self.assertIn('NotebookApp::run', function_names)
        self.assertIn('NotebookApp::printMenu', function_names)
        self.assertIn('NotebookApp::addNote', function_names)
        self.assertIn('NotebookApp::listNotes', function_names)
        self.assertIn('NotebookApp::deleteNote', function_names)
        self.assertIn('NotebookApp::exitApp', function_names)
        self.assertIn('NotebookApp::saveNotes', function_names)
        self.assertIn('NotebookApp::loadNotes', function_names)
        self.assertNotIn('foreach', function_names)

    def test_modern_php8_features(self):
        php_code = '''<?php
        trait Loggable {
            public function log(string $message): void {
                echo date('Y-m-d H:i:s') . ": $message\\n";
            }
        }

        class Product {
            use Loggable;

            public function __construct(
                private string $name,
                private float $price,
                private ?int $stock = null
            ) {
                $this->log("Created new product: $name");
            }

            public function applyDiscount(float $percent): float {
                $calculate = fn($price, $discount) => $price * (1 - $discount/100);
                return $calculate($this->price, $percent);
            }

            public function getDetails(bool $includeStock = false): string|array {
                if ($includeStock) {
                    return [
                        'name' => $this->name,
                        'price' => $this->price,
                        'stock' => $this->stock ?? 'Unknown'
                    ];
                }
                return "{$this->name}: ${$this->price}";
            }

            public function getAvailability(): string {
                return match(true) {
                    $this->stock === null => 'Unknown',
                    $this->stock <= 0 => 'Out of stock',
                    $this->stock < 5 => 'Low stock',
                    $this->stock >= 5 => 'In stock',
                    default => 'Status unknown'
                };
            }

            public function processOrder(int $quantity, bool $express = false): void {
                $this->log(message: "Processing order for {$quantity} x {$this->name}");

                if ($express) {
                    if ($quantity > 10 && $this->stock !== null && $this->stock >= $quantity) {
                        $this->log("Express shipping approved");
                    } else if ($quantity <= 10 || $this->stock === null) {
                        $this->log("Standard shipping applied");
                    }
                }
            }
        }

        $product = new Product(
            name: "PHP 8 Guide",
            price: 29.99
        );
        ?>'''

        functions = get_php_function_list(php_code)
        function_names = sorted(f.name for f in functions)

        self.assertIn('Loggable::log', function_names)
        self.assertIn('Product::__construct', function_names)
        self.assertIn('Product::applyDiscount', function_names)
        self.assertIn('Product::getDetails', function_names)
        self.assertIn('Product::getAvailability', function_names)
        self.assertIn('Product::processOrder', function_names)
        self.assertEqual(6, len(functions))

        match_function = next(f for f in functions if f.name == 'Product::getAvailability')
        self.assertEqual(2, match_function.cyclomatic_complexity)

        process_order = next(f for f in functions if f.name == 'Product::processOrder')
        self.assertEqual(7, process_order.cyclomatic_complexity)
