import asyncio
import pandas as pd
from datetime import datetime
from playwright.async_api import async_playwright
import random
import re

async def collect_data():
    print("Starting CIAN parser (Krasnodar) with playwright...")
    all_flats = []
    page_num = 1
    MIN_RECORDS = 200
    MAX_PAGES = 30
    
    KRASNODAR_REGION_ID = "4820"

    async with async_playwright() as p:
        browser = await p.chromium.launch(
            headless=False,
            args=['--disable-blink-features=AutomationControlled', '--no-sandbox']
        )
        
        context = await browser.new_context(
            viewport={'width': 1920, 'height': 1080},
            user_agent='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36',
            locale='ru-RU',
            timezone_id='Europe/Moscow'
        )
        
        await context.add_init_script("""
            Object.defineProperty(navigator, 'webdriver', { get: () => undefined });
            window.chrome = { runtime: {} };
        """)
        
        page = await context.new_page()

        while len(all_flats) < MIN_RECORDS and page_num <= MAX_PAGES:
            print(f"\n{'='*60}")
            print(f"Processing page {page_num} | Collected: {len(all_flats)}")
            print(f"{'='*60}")

            url = f"https://krasnodar.cian.ru/cat.php?deal_type=sale&engine_version=2&offer_type=flat&p={page_num}&region={KRASNODAR_REGION_ID}"
            
            try:
                print(f"Loading: {url}")
                
                await page.goto(url, wait_until="domcontentloaded", timeout=30000)
                await page.wait_for_timeout(random.randint(3000, 5000))
                
                cards = await page.query_selector_all('article[data-name="CardComponent"]')
                print(f"Found {len(cards)} cards")
                
                if not cards:
                    print(f"No listings on page {page_num}")
                    break
                
                page_flats = []
                for idx, card in enumerate(cards):
                    try:
                        flat = {}
                        
                        card_text = await card.inner_text()
                        
                        link = await card.query_selector('a[href*="/sale/flat/"]')
                        if not link:
                            link = await card.query_selector('a')
                        
                        if link:
                            href = await link.get_attribute('href')
                            if href:
                                if href.startswith('/'):
                                    flat['url'] = f"https://cian.ru{href}"
                                else:
                                    flat['url'] = href
                            else:
                                continue
                        else:
                            continue
                        
                        price_match = None
                        
                        lines = card_text.split('\n')
                        for line in lines:
                            if '₽' in line and '/м²' not in line:
                                price_in_line = re.search(r'(\d[\d\s]*)\s*₽', line)
                                if price_in_line:
                                    price_str = re.sub(r'\s', '', price_in_line.group(1))
                                    if price_str.isdigit():
                                        price_match = price_str
                                        break
                        
                        if not price_match:
                            all_prices = re.findall(r'(\d[\d\s]*)\s*₽', card_text)
                            if all_prices:
                                for price_str in all_prices:
                                    clean_price = re.sub(r'\s', '', price_str)
                                    if clean_price.isdigit():
                                        price_match = clean_price
                                        break
                        
                        if price_match:
                            flat['price_rub'] = int(price_match)
                        else:
                            continue
                        
                        rooms_match = re.search(r'(\d+)-комн', card_text)
                        if rooms_match:
                            flat['rooms'] = int(rooms_match.group(1))
                        elif 'студия' in card_text.lower():
                            flat['rooms'] = 0
                        else:
                            flat['rooms'] = None
                        
                        area_match = re.search(r'(\d+[.,]?\d*)\s*м²', card_text)
                        if area_match:
                            try:
                                flat['total_area_m2'] = float(area_match.group(1).replace(',', '.'))
                            except:
                                flat['total_area_m2'] = None
                        else:
                            flat['total_area_m2'] = None
                        
                        address_match = re.search(r'Краснодарский край, Краснодар, ([^,\n]+)', card_text)
                        if address_match:
                            flat['address'] = f"Краснодар, {address_match.group(1)}"
                        else:
                            district_match = re.search(r'Краснодар[^,\n]*,\s*([^,\n]+)', card_text)
                            if district_match:
                                flat['address'] = f"Краснодар, {district_match.group(1)}"
                            else:
                                flat['address'] = "Краснодар"
                        
                        floor_match = re.search(r'(\d+)/(\d+)\s+этаж', card_text)
                        if not floor_match:
                            floor_match = re.search(r'(\d+)/(\d+)\s+эт', card_text)
                        if floor_match:
                            flat['floor'] = int(floor_match.group(1))
                            flat['floors_total'] = int(floor_match.group(2))
                        else:
                            flat['floor'] = None
                            flat['floors_total'] = None
                        
                        page_flats.append(flat)
                        print(f"{flat['price_rub']:,} RUB | {flat.get('rooms', '?')} rooms | {flat.get('total_area_m2', '?')} sqm")
                        
                    except Exception as e:
                        print(f"Error in card {idx}: {e}")
                        continue
                
                all_flats.extend(page_flats)
                print(f"\nAdded {len(page_flats)} listings (total {len(all_flats)})")
                
                if len(page_flats) == 0:
                    print(f"No parsable listings on page {page_num}")
                
                page_num += 1
                
                delay = random.randint(5000, 10000)
                print(f"Pausing {delay//1000} seconds...")
                await page.wait_for_timeout(delay)
                
            except Exception as e:
                print(f"Error on page {page_num}: {e}")
                await page.wait_for_timeout(15000)
                page_num += 1
                continue

        await browser.close()
        
        if all_flats:
            df = pd.DataFrame(all_flats)
            
            columns_order = ['url', 'price_rub', 'address', 'rooms', 'total_area_m2', 'floor', 'floors_total']
            df = df[[col for col in columns_order if col in df.columns]]
            df = df.drop_duplicates(subset=['url'])
            df = df.sort_values('price_rub')
            
            filename = f"cian_krasnodar_flats_{len(df)}_{datetime.now().strftime('%Y%m%d_%H%M')}.csv"
            df.to_csv(filename, index=False, encoding="utf-8-sig")
            
            print(f"\n{'='*60}")
            print(f"Completed. Collected {len(df)} records")
            print(f"File: {filename}")
            print(f"\nStatistics:")
            print(f"Price range: {df['price_rub'].min():,} - {df['price_rub'].max():,} RUB")
            print(f"Mean price: {df['price_rub'].mean():,.0f} RUB")
            print(f"\nFirst 5 records:")
            print(df[['price_rub', 'rooms', 'total_area_m2', 'address']].head())
            print(f"\nExample URL: {df['url'].iloc[0]}")
            print(f"{'='*60}")
        else:
            print("\nNo data collected")

asyncio.run(collect_data())