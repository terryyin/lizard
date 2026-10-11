"""JavaScript widget source examples used by member-detection regressions."""


FULL_INTERACTIVE_WIDGET = '''
        /**
         * Interactive UI Component with Multiple JavaScript Methods
         */
        class InteractiveWidget {
            constructor(containerId) {
                this.container = document.getElementById(containerId);
                if (!this.container) {
                    throw new Error(`Container element with ID ${containerId} not found`);
                }
                this.state = {
                    clicks: 0,
                    items: [],
                    timer: null,
                    isRunning: false
                };
                this.init();
            }
            // Initialization methods
            init() {
                this.render();
                this.bindEvents();
            }
            render() {
                this.container.innerHTML = '';
                this.updateUI();
            }
            updateUI() {
                document.getElementById('click-count').textContent = this.state.clicks;
                this.renderItemList();
            }
            renderItemList() {
                const list = document.getElementById('item-list');
                list.innerHTML = this.state.items.map(item => `<li>${item}</li>`).join('');
            }
            bindEvents() {
                document.getElementById('click-btn').addEventListener('click', this.handleClick.bind(this));
            }
            handleClick() {
                this.state.clicks++;
                this.updateUI();
            }
            toggleTimer() {
                if (this.state.isRunning) {
                    this.stopTimer();
                } else {
                    this.startTimer();
                }
                this.state.isRunning = !this.state.isRunning;
            }
            startTimer() {
                this.state.timerCount = 0;
                this.state.timer = setInterval(() => {
                    this.state.timerCount++;
                    this.updateUI();
                }, 1000);
            }
            stopTimer() {
                clearInterval(this.state.timer);
                this.state.timer = null;
            }
            addItem() {
                const input = document.getElementById('item-input');
                const value = input.value.trim();
                if (value) {
                    this.state.items.push(value);
                    input.value = '';
                    this.updateUI();
                }
            }
            removeItem(item) {
                this.state.items = this.state.items.filter(i => i !== item);
                this.updateUI();
            }
            animateButton(buttonId) {
                const button = document.getElementById(buttonId);
                button.classList.add('clicked');
                setTimeout(() => {
                    button.classList.remove('clicked');
                }, 200);
            }
            animateAddition() {
                const list = document.getElementById('item-list');
                list.classList.add('item-added');
                setTimeout(() => {
                    list.classList.remove('item-added');
                }, 300);
            }
            static formatDate(date) {
                return new Intl.DateTimeFormat('en-US', {
                    year: 'numeric',
                    month: 'long',
                    day: 'numeric',
                    hour: '2-digit',
                    minute: '2-digit'
                }).format(date);
            }
            static generateRandomId() {
                return Math.random().toString(36).substring(2, 9);
            }
            static async simulateApiCall(data) {
                return new Promise((resolve) => {
                    setTimeout(() => {
                        resolve({
                            status: 'success',
                            data,
                            timestamp: new Date().toISOString(),
                            id: this.generateRandomId()
                        });
                    }, 1000);
                });
            }
            static processItems(items, processorFn) {
                return items.map((item, index) => processorFn(item, index));
            }
            static filterUnique(array) {
                return [...new Set(array)];
            }
            static sortByKey(array, key, ascending = true) {
                return [...array].sort((a, b) => {
                    if (a[key] < b[key]) return ascending ? -1 : 1;
                    if (a[key] > b[key]) return ascending ? 1 : -1;
                    return 0;
                });
            }
        }
        '''



INTERACTIVE_WIDGET = '''
        class InteractiveWidget {
            constructor(containerId) {
                this.container = document.getElementById(containerId);
                if (!this.container) {
                    throw new Error(`Container element with ID ${containerId} not found`);
                }
                this.state = { clicks: 0, items: [], timer: null };
                this.init();
            }

            init() {
                this.render();
            }

            render() {
                this.container.innerHTML = `<div>widget</div>`;
                this.updateUI();
            }

            updateUI() {
                document.getElementById('count').textContent = this.state.clicks;
            }

            handleClick() {
                this.state.clicks++;
                this.updateUI();
            }

            startTimer() {
                this.state.timer = setInterval(() => {
                    this.state.count++;
                }, 1000);
            }

            stopTimer() {
                clearInterval(this.state.timer);
            }

            static formatDate(date) {
                return new Date().toISOString();
            }

            static generateRandomId() {
                return Math.random().toString(36).substring(2, 9);
            }

            static async simulateApiCall(data) {
                return new Promise((resolve) => {
                    setTimeout(() => {
                        resolve({
                            status: 'success',
                            data,
                            timestamp: new Date().toISOString(),
                            id: this.generateRandomId()
                        });
                    }, 1000);
                });
            }

            static processItems(items, processorFn) {
                return items.map((item, index) => processorFn(item, index));
            }

            static filterUnique(array) {
                return [...new Set(array)];
            }
        }
        '''
