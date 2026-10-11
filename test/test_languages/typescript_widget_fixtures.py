"""TypeScript widget source example used by member-detection regressions."""


INTERACTIVE_WIDGET = '''
        class InteractiveWidget {
            private container: HTMLElement;
            private state: any;

            constructor(containerId: string) {
                this.container = document.getElementById(containerId) as HTMLElement;
                if (!this.container) {
                    throw new Error(`Container element with ID ${containerId} not found`);
                }
                this.state = { clicks: 0, items: [], timer: null };
                this.init();
            }

            init(): void {
                this.render();
            }

            render(): void {
                this.container.innerHTML = `<div>widget</div>`;
                this.updateUI();
            }

            updateUI(): void {
                (document.getElementById('count') as HTMLElement).textContent = this.state.clicks.toString();
            }

            handleClick(): void {
                this.state.clicks++;
                this.updateUI();
            }

            startTimer(): void {
                this.state.timer = setInterval(() => {
                    this.state.count++;
                }, 1000);
            }

            stopTimer(): void {
                clearInterval(this.state.timer);
            }

            static formatDate(date: Date): string {
                return new Date().toISOString();
            }

            static generateRandomId(): string {
                return Math.random().toString(36).substring(2, 9);
            }

            static async simulateApiCall(data: any): Promise<any> {
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

            static processItems(items: string[], processorFn: Function): any[] {
                return items.map((item, index) => processorFn(item, index));
            }

            static filterUnique<T>(array: T[]): T[] {
                return [...new Set(array)];
            }
        }
        '''
